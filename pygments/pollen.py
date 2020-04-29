from pygments.lexer import *
from pygments.token import *
from pygments.lexers.lisp import RacketLexer
import re

class PollenLexer(RegexLexer):
    """
    Lexer for Pollen
    """

    name = 'Pollen'
    aliases = ['pollen']
    filenames = ['*.html.pm']

    flags = re.IGNORECASE | re.DOTALL | re.MULTILINE

    valid_symbol_chars = r'[\w!$%*+,<=>?/.\'@&#:-]'
    variable = r'[A-Z]%s*' % valid_symbol_chars

    tokens = {
        'root': [
            (r'◊;.*?$', Comment),
            ('◊', Name.Variable.Magic, 'post-magic'),
            (r'.', Text)
        ],

        'post-magic': [
            (r'(\|)(%s)(\|)' % variable,
                bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic),
                '#pop'),
            (r'(\()(.+)(\))',
                bygroups(Name.Variable.Magic,
                         using(RacketLexer, state='unquoted-datum'),
                         Name.Variable.Magic),
                '#pop'),
            (r'%s' % variable, Name.Variable, ('#pop', 'post-var')),
        ],

        'post-var': [
            (r'(\[)(.+?)(\])',
                bygroups(Name.Variable.Magic,
                         using(RacketLexer, state='unquoted-datum'),
                         Name.Variable.Magic),
                ('#pop', 'curly-start')),
            include('curly-start'),
        ],

        'curly-start': [
            (r'\{', Name.Variable.Magic, ('#pop', 'curly-end'))
        ],

        'curly-end': [
            (r'\}', Name.Variable.Magic, '#pop'),
            include('root'),
        ],
    }

