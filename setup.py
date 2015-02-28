# -*- coding: utf-8 -*-
import sys

from setuptools import setup
from setuptools.command.test import test


class Tox(test):
    def initialize_options(self):
        test.initialize_options(self)
        self.tox_args = None

    def finalize_options(self):
        test.finalize_options(self)
        self.test_args = []
        self.test_suite = True

    def run_tests(self):
        import tox
        sys.exit(tox.cmdline())

if __name__ == '__main__':
    setup(
        name='match.py',
        version='0.1',
        description='Match is a library that works on a small subset of regular expressions for fast url matching.',
        url='https://github.com/hackaugusto/match.py',
        author='Augusto F. Hack',
        author_email='hack.augusto@gmail.com',
        license='MIT',

        py_modules=['timer'],
        classifiers=[
            'Development Status :: 4 - Beta',
            'Intended Audience :: Developers',
            'Programming Language :: Python :: 2.7',
            'Programming Language :: Python :: 3.4',
        ],
        keywords=['timer'],
        tests_require=['tox'],
        cmdclass={'test': Tox},
    )
