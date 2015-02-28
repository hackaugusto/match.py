# Match.py

> This is a work in progress

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
