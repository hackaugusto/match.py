# -*- coding: utf8 -*-
'''
Match is a small subset of regular expressions, the library will generate a
DFA for a collection of expressions and on a given match it will give results
aboute which expression was used.

Match only accepts a subset of regex, these features are *NOT* supported:

    - Patterns that match in the middle of a string (all patterns must be
      anchored with ^)
    - Look-ahead (?=) or (?!) and look-behind (?=<) or (?!<) assertions
    - Non-greedy operators
    - back-references like \\n and (P=)
    - posix character classes [[:alpha:]]
    - flags for multi-line and case-sensitivity
    - boundary matching with escape sequences (\\b, \\B, etc.)
    - comentaries (?#)
    - yes-no patterns (?(id/name)yes|no)
    - especial treatment for [-], []], and [^-]
    - end of string anchor $

This *ARE* supported:

    - Character classes [a-z0-9]
    - Repetition with ? * + and {,}
    - groups and namedgroups (|) or (?<url>)
    - non-capturing groups (?:)

Notes:

    - We only accept regexes anchored with '^' because otherwise we wouldn't be
      capable of merging them
    - We don't accept the anchor for end-of-string because we are targeting
      HTTP that uses '\\r\\n' to express end of the line
'''

import unittest

from collections import namedtuple
from functools import partial

# Represents any value "."
Any = object()

# Classes used to represent the posible repetitions "{}", "+", "*", "?"
Repeat = namedtuple('Repeat', ('pattern', 'minimum', 'maximum'))
Star = type('Star', (Repeat,), {'__new__': partial(Repeat.__new__, minimum=0, maximum=float('inf'))})
Plus = type('Plus', (Repeat,), {'__new__': partial(Repeat.__new__, minimum=1, maximum=float('inf'))})
Maybe = type('Maybe', (Repeat,), {'__new__': partial(Repeat.__new__, minimum=0, maximum=1)})

# Represents a character class
Range = namedtuple('Range', ('start', 'end'))

# useful to report recursive erros with a good context (we are not using
# recursion any more, but keep the class just in case)
Error = namedtuple('Error', ('message', 'consumed'))


# every item in the dictionary represents a option in a group, eg.:
#   [['a]', ['b']] for "(a|b)"
class Group(list):
    def __init__(self, name, capture, *args, **kwargs):
        self.name = name
        self.capture = capture
        super(Group, self).__init__(*args, **kwargs)


class Class(list):
    def __init__(self, negated, *args, **kwargs):
        self.negated = negated
        super(Class, self).__init__(*args, **kwargs)


class Track(object):
    '''Track is used to know how much of the original string was consumed to
    produce better error messages and peek

    In python 3 we could simply use a closure with nonlocal, but python 2.7
    doesn't have nonlocal.
    '''
    def __init__(self, iterator):
        self.consumed = 0
        self.iterator = iterator

    def next(self):
        try:
            return next(self.iterator)
        finally:
            self.consumed += 1

    def __iter__(self):
        return self


def pattern(pattern):
    if not isinstance(pattern, basestring):
        raise Exception('pattern must be a string')

    if pattern.startswith('^'):
        # we make the anchoring required because it makes sense for stream
        # matching and makes merging easier
        raise Exception('Only patterns anchored with "^" are accepted')

    result = parse(pattern)

    if isinstance(result, Error):
        raise Exception('{}, error near: {}'.format(result.message, result.consumed))

    return result


def parse(pattern):
    '''Parses the regular expression.

    Simple expressions are represented as a list of values:
        'any'               = ['a', 'n', 'y']

    The metacharacter . is represented with the object Any:
        'a.y'               = ['a', Any, 'y']

    Character clases are represented with `Class` and may contain `Range`s:
        '[any]'             = [Class(['a', 'n', 'y'])]
        '[a-ny]'            = [Class([Range('a', 'n'), 'y'])]

    Repetition metacharacters add annotations:
        'any?'              = ['a', 'n', Maybe('y')]
        'any*'              = ['a', 'n', Star('y')]
        '[any]+'            = [Plus(['a', 'n', 'y'])]

    Groups add a new list where each item is an alternative:
        'a|ny'              = Group(name=None, capture=True, [['a'], ['n', 'y']])
        '(?:a|ny){3,5}'     = Repeat(Group(name=None, capture=False, [['a'], ['n', 'y']]), 3, 5)
        '(?<match>a|ny)'    = Group(name='match', capture=True, [['a'], ['n', 'y']])

    Keep in mind that the full expression is considered a group, so a
    expression 'any' will actually result in:
        'any'               = Group([['a', 'n', 'y']])
    '''
    # iterator is used because we can consume the input in inner loops
    iterator = Track(iter(pattern))

    def peek(chars=1):
        pos = iterator.consumed + chars - 1

        if pos < len(pattern):
            return pattern[pos]

        return None

    # flag used when a escaped character is used, eg.: '\\'
    escape = False

    # a list of the named groups, used to prohibit duplicated names
    group_set = set()

    # stack for keeping nested groups, when a new inner group is open, the
    # state of the outter group is store is a tuple:
    #   (alternatives, consume, groupname, groupcapture)
    nested = []

    # hold the current simple expression, ie one alternative of a group
    parsed = []

    # hold all alternatives of the group
    alternatives = []
    alternatives.append(parsed)

    # current group name
    groupname = None

    # flag indicating if we should capture or not the value (start with True
    # because we want to capture the whole match)
    groupcapture = True

    for char in iterator:
        if escape:
            escape = False

            if char in '1234567890':
                return Error('backtracking is not supported', pattern[:iterator.consumed])

            parsed.append(char)

        elif char == '\\':
            escape = True

        elif char == '.':
            parsed.append(Any)

        elif char == '+':
            last = parsed.pop()

            if isinstance(last, Repeat):
                return Error('cannot use {} after {}'.format(char, last), pattern[:iterator.consumed])

            if isinstance(last, Group) and last.name is not None:
                return Error('cannot repeat a named group', pattern[:iterator.consumed])

            parsed.append(Plus(last))

        elif char == '*':
            last = parsed.pop()

            if isinstance(last, Repeat):
                return Error('cannot use {} after {}'.format(char, last), pattern[:iterator.consumed])

            if isinstance(last, Group) and last.name is not None:
                return Error('cannot repeat a named group', pattern[:iterator.consumed])

            parsed.append(Star(last))

        elif char == '?':
            last = parsed.pop()

            if isinstance(last, Repeat):
                return Error('non greedy repetition is no supported', pattern[:iterator.consumed])

            # ? can accept named group because at most there will be one match

            parsed.append(Maybe(last))

        elif char == '{':
            last = parsed.pop()

            if isinstance(last, Repeat):
                return Error('cannot use {} after {}'.format(char, last), pattern[:iterator.consumed])

            if isinstance(last, Group) and last.name is not None:
                return Error('cannot repeat a named group', pattern[:iterator.consumed])

            # we use None to distinguish between not have a value parsed yet
            # from a empty value
            minimum, maximum, current = None, None, ''
            for other in iterator:
                # the user must escape `{` if it is not declaring a repetition
                if other not in '1234567890,}':
                    return Error('invalid character inside {', pattern[:iterator.consumed])

                if other == ',':
                    # we already have a minimum parsed (it's value is not None),
                    # this means that we have more than one comma, eg.:
                    #   {1,2,3} or {,,}
                    if minimum is not None:
                        return Error('too many commas inside {', pattern[:iterator.consumed])

                    # empty lower bound is permitted, eg.: {,5}
                    minimum = current
                    # reset current, now it is going to hold the maxium
                    current = ''
                elif other == '}':
                    # empty upper bound is permitted, eg.: {3,}
                    maximum = current
                    current = ''
                    break
                else:
                    current += other

            # end of stream
            if other != '}':
                return Error('repeat missing the ending }', pattern[:iterator.consumed])

            # at least the lower or upper bound must be informed, cannot use {,}
            if not minimum and not maximum:
                return Error('empty {,}', pattern[:iterator.consumed])

            # the parser only accept numbers, so we know this will not raise
            minimum = int(minimum) if minimum else 0
            maximum = int(maximum) if maximum else float('inf')

            if minimum and maximum and minimum > maximum:
                msg = 'range {{{},{}}} has the start larger than the end'.format(minimum, maximum)
                return Error(msg, pattern[:iterator.consumed])

            parsed.append(Repeat(last, minimum, maximum))

        elif char == '|':
            # empty alternative at the beginning, end or middle of the pattern, eg.: a||b
            if len(parsed) == 0:
                return Error('empty alternative', pattern[:iterator.consumed])

            parsed = []
            alternatives.append(parsed)

        elif char == ')':
            subgroup = Group(groupname, groupcapture, alternatives)

            # restore the outter group state
            alternatives, __, groupname, groupcapture = nested.pop()

            # save inner group pattern
            parsed = alternatives[-1]
            parsed.append(subgroup)

        elif char == '(':
            # we are opening a new nested group, store the current outter group
            # in the stack and start a new simple expression
            nested.append((alternatives, iterator.consumed, groupname, groupcapture))

            # we don't need to change `consume` since it is a integer and
            # integers are imutable
            groupname = None
            groupcapture = True
            parsed = []
            alternatives = []
            alternatives.append(parsed)

            groupstart = iterator.consumed
            strpeek = pattern[groupstart:groupstart+3]

            if strpeek.startswith('?P='):
                return Error('named group backreferences are not supported', pattern[:groupstart])

            elif strpeek.startswith('?P<'):
                next(iterator)  # ?
                next(iterator)  # P
                next(iterator)  # <

                groupname = ''
                for other in iterator:
                    if other == '>':
                        break
                    groupname += other

                # end of stream
                if other != '>':
                    return Error('named group missing the ending >', pattern[:groupstart])

                if not groupname:
                    return Error('group name is empty', pattern[:groupstart])

                if groupname in group_set:
                    return Error('repeated group name {}'.format(groupname), pattern[:groupstart])

                group_set.add(groupname)

            elif strpeek.startswith('?:'):
                next(iterator)  # ?
                next(iterator)  # :
                groupcapture = False

            elif strpeek.startswith('?#'):
                return Error('commentaries (?#) are not supported', pattern[:groupstart])

            elif strpeek.startswith('?!') or strpeek.startswith('?=') or strpeek.startswith('?<'):
                return Error('assertions (?=) and (?!) is not supported', pattern[:groupstart])

            elif strpeek.startswith('?'):
                return Error('unsuported feature (?{})'.format(peek(2)), pattern[:groupstart])

        elif char == '[':
            setstart = iterator.consumed
            values = []
            setnegated = False

            if peek() == '^':
                next(iterator)
                setnegated = True

            if peek() == ']':
                msg = 'empty character class (the character ] must be escaped)'
                return Error(msg, pattern[:setstart + iterator.consumed])

            # for the sake of simplicity, we do not accept these expressions:
            #   []]
            #   [-a-z]
            # instead use:
            #   [\]]
            #   [\-a-z]
            #
            # XXX: handle escaped sequences
            for other in iterator:

                if other == '-':
                    msg = 'missing left side of the range - (the character - must be escaped)'
                    return Error(msg, pattern[:iterator.consumed])

                if other == ']':
                    break

                if other == '\\' and peek() == 'd':
                    other = next(iterator)

                    values.append(Range('0', '9'))

                elif other == '\\' and peek() == 's':
                    other = next(iterator)

                    # horizontal tab + new line + vertical tab + form feed + carriage return
                    values.append(Range('\t', '\r'))

                elif other == '\\' and peek() == 'w':
                    other = next(iterator)

                    values.append(Range('a', 'z'))
                    values.append(Range('A', 'Z'))

                elif peek() == '-':
                    next(iterator)

                    first, second = other, next(iterator)

                    # [a-] or [\--]
                    if second == ']':
                        return Error('missing right side of the range -', pattern[:iterator.consumed])

                    if second <= first:
                        msg = 'invalid range, second value is smaller than the first'
                        return Error(msg, pattern[:iterator.consumed])

                    values.append(Range(first, second))
                else:
                    values.append(other)

            # end of stream
            if other != ']':
                return Error('repeat missing the ending ]', pattern[:iterator.consumed])

            parsed.append(Class(setnegated, values))
        elif char in ')}]':
            # python's re module only raise on the parenthesis
            msg = 'unbalanced {} (the character must be escaped to be used alone)'.format(char)
            return Error(msg, pattern[:iterator.consumed])

        else:
            parsed.append(char)

    if nested:
        total = len(nested)
        __, groupstart, __, __ = nested[-1]
        return Error('there are unclosded groups'.format(total), pattern[:groupstart])

    return Group(groupname, groupcapture, alternatives)


def canonical(pattern):
    '''Expand the repetitions'''


def optimize(pattern):
    '''join chars into a string for matching'''


class PatternTestCase(unittest.TestCase):
    def test_only_anchored(self):
        with self.assertraises(Exception):
            pattern(r'woops')

    def test_no_backtracking(self):
        with self.assertraises(Exception):
            pattern(r'^(a)\1')

        with self.assertraises(Exception):
            pattern(r'^a+?')

        with self.assertraises(Exception):
            pattern(r'^a*?')

    def test_no_nongreedy(self):
        with self.assertraises(Exception):
            pattern(r'^a+?')

        with self.assertraises(Exception):
            pattern(r'^a*?')

        with self.assertraises(Exception):
            pattern(r'^a??')

        with self.assertraises(Exception):
            pattern(r'^a{1,2}?')

    def test_no_assertions(self):
        with self.assertraises(Exception):
            pattern(r'^(?=oops)')

        with self.assertraises(Exception):
            pattern(r'^(?<oops)')

        with self.assertraises(Exception):
            pattern(r'^(?!oops)')

        with self.assertraises(Exception):
            pattern(r'^(?!<oops)')

    def test_no_atomic_groups(self):
        with self.assertraises(Exception):
            pattern(r'^(?>oops)')

    def test_invalid_character_classes(self):
        with self.assertraises(Exception):
            pattern(r'^[-]')

        with self.assertraises(Exception):
            pattern(r'^[]')

        with self.assertraises(Exception):
            pattern(r'^[^]')

        with self.assertraises(Exception):
            pattern(r'^[^-]')

    def test_empty_range_character_classes(self):
        with self.assertraises(Exception):
            pattern(r'^[-a]')

        with self.assertraises(Exception):
            pattern(r'^[b-]')

        with self.assertraises(Exception):
            pattern(r'^[^-a]')

        with self.assertraises(Exception):
            pattern(r'^[^b-]')

        with self.assertraises(Exception):
            pattern(r'^[a-de-]')

        with self.assertraises(Exception):
            pattern(r'^[a-e-f]')

        with self.assertraises(Exception):
            pattern(r'^[^\--]')

        with self.assertraises(Exception):
            pattern(r'^[^-\-]')

    def test_dont_accept_sequential_repetitions(self):
        # Plus
        with self.assertraises(Exception):
            pattern(r'^a++')

        with self.assertraises(Exception):
            pattern(r'^a*+')

        with self.assertraises(Exception):
            pattern(r'^a?+')

        with self.assertraises(Exception):
            pattern(r'^a{1,2}+')

        # Star
        with self.assertraises(Exception):
            pattern(r'^a+*')

        with self.assertraises(Exception):
            pattern(r'^a**')

        with self.assertraises(Exception):
            pattern(r'^a?*')

        with self.assertraises(Exception):
            pattern(r'^a{1,2}*')

        # Repeat
        with self.assertraises(Exception):
            pattern(r'^a+{1,2}')

        with self.assertraises(Exception):
            pattern(r'^a*{1,2}')

        with self.assertraises(Exception):
            pattern(r'^a?{1,2}')

        with self.assertraises(Exception):
            pattern(r'^a{1,2}{1,2}')


class MatchTestCase(unittest.TestCase):
    def test_one_letter(self):
        re = pattern(r'^a')

        self.asserttrue(re.match('a'))
        self.asserttrue(re.match('ab'), 'we force the anchor just at the start')

        self.assertfalse(re.match('b'))

    def test_maybe(self):
        re = pattern(r'^a?')

        self.asserttrue(re.match('a'))
        self.asserttrue(re.match('b'))

    def test_maybe_at_end(self):
        re = pattern(r'^ab?')

        self.asserttrue(re.match('a'))
        self.asserttrue(re.match('ab'))

        self.assertfalse(re.match('b'))

    def test_maybe_at_start(self):
        re = pattern(r'^a?b')

        self.asserttrue(re.match('b'))
        self.asserttrue(re.match('ab'))

        self.assertfalse(re.match('a'))
        self.assertfalse(re.match('cb'))

    def test_maybe_in_the_middle(self):
        re = pattern(r'^ab?c')

        self.asserttrue(re.match('ac'))
        self.asserttrue(re.match('abc'))

        self.assertfalse(re.match('a'))
        self.assertfalse(re.match('c'))
        self.assertfalse(re.match('b'))
        self.assertfalse(re.match('ab'))
        self.assertfalse(re.match('bc'))


class MatchOptimizationTestCase(unittest.TestCase):
    def test_maybe_at_end(self):
        self.assertequal(pattern(''), optimize(pattern('^a?')))
        self.assertequal(pattern('^a'), optimize(pattern('^ab?')))

    def test_plus_then_maybe(self):
        self.assertequal(pattern('^a+'), optimize(pattern('^a+a?')))

    def test_plus_then_star(self):
        self.assertequal(pattern('^a+'), optimize(pattern('^a+a*')))

    def test_star_then_maybe(self):
        self.assertequal(pattern('^a*'), optimize(pattern('^a*a?')))

    def test_star_then_char(self):
        self.assertequal(pattern('^a+'), optimize(pattern('^a*a')))

    def test_star_then_plus(self):
        self.assertequal(pattern('^a+'), optimize(pattern('^a*a+')))

    def test_group_start(self):
        self.assertequal(pattern('^a(b|c)'), optimize(pattern('^(ab|ac)')))
        self.assertequal(pattern('^a+(b|c)'), optimize(pattern('^(a+b|a+c)')))

        self.assertnotequal(pattern('^a+(b|c)'), optimize(pattern('^(ab|a+c)')))
        self.assertnotequal(pattern('^a+(b|c)'), optimize(pattern('^(a+b|ac)')))

    def test_group_end(self):
        self.assertequal(pattern('^(a|b)c'), optimize(pattern('^(ac|bc)')))
        self.assertequal(pattern('^(a|b)c+'), optimize(pattern('^(ac+|bc+)')))

        self.assertnotequal(pattern('^(a|b)c+'), optimize(pattern('^(ac|bc+)')))
        self.assertNotEqual(pattern('^(a|b)c+'), optimize(pattern('^(ac+|bc)')))
