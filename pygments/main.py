# -*- coding: utf-8 -*-
# Mostly from https://github.com/blaenk/blaenk.github.io
from __future__ import print_function

import sys

from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import get_lexer_by_name
from pygments.util import ClassNotFound

from pollen import PollenLexer

html = HtmlFormatter(encoding='utf-8', nowrap=True)

def eprint(err):
    with open('pygments-fail', 'a') as myfile:
        myfile.write(err)
    sys.exit()


while True:
    try:
        lang = sys.stdin.readline().rstrip("\n")
        amt = int(sys.stdin.readline().rstrip("\n"))
        code = sys.stdin.read(amt)

        rv = ""
        try:
            try:
                if lang == "pollen":
                    lex = PollenLexer(encoding="utf-8")
                else:
                    lex = get_lexer_by_name(lang, encoding="utf-8")
            except ClassNotFound as err:
                eprint("Unknown language: {}\n".format(lang))
                lex = get_lexer_by_name("text", encoding="utf-8")

            rv = highlight(code, lex, html).rstrip()
        except ValueError as err:
            rv = "Pygments Error: {}".format(err)

        sys.stdout.write(str(len(rv)))
        sys.stdout.write("\n")
        sys.stdout.flush()

        if not hasattr(sys.stdout, 'buffer'):
            sys.stdout.write(rv)
            sys.stdout.flush()
        else:
            sys.stdout.buffer.write(rv)
            sys.stdout.buffer.flush()
    except Exception as err:
        # Should hopefully never happen
        eprint("Uncaught error: {}\n".format(err))
        sys.exit()

