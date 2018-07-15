﻿from enum import IntEnum

keywords = [ # https://docs.python.org/3/reference/lexical_analysis.html#keywords
            'False',      'await',      'else',       'import',     'pass',
            'None',       'break',      'except',     'in',         'raise',
            'True',       'class',      'finally',    'is',         'return',
            'and',        'continue',   'for',        'lambda',     'try',
            'as',         'def',        'from',       'nonlocal',   'while',
            'assert',     'del',        'global',     'not',        'with',
            'async',      'elif',       'if',         'or',         'yield',]

operators = [ # https://docs.python.org/3/reference/lexical_analysis.html#operators
             '+',       '-',       '*',       '**',      '/',       '//',      '%',      '@',
             '<<',      '>>',      '&',       '|',       '^',       '~',
             '<',       '>',       '<=',      '>=',      '==',      '!=',]
#operators.sort(key = lambda x: len(x), reverse = True)

delimiters = [ # https://docs.python.org/3/reference/lexical_analysis.html#delimiters        
              '(',       ')'       '[',       ']',       '{',       '}',
              ',',       ':'       '.',       ';',       '@',       '=',       '->',
              '+=',      '-='      '*=',      '/=',      '//=',     '%=',      '@=',
              '&=',      '|='      '^=',      '>>=',     '<<=',     '**=',]
#delimiters.sort(key = lambda x: len(x), reverse = True)
operators_and_delimiters = sorted(operators + delimiters, key = lambda x: len(x), reverse = True)

class Exception(Exception):
    def __init__(self, message, pos):
        self.message = message
        self.pos = pos

class Token:
    class Category(IntEnum):
        IDENTIFIER = 0
        KEYWORD = 1
        OPERATOR_OR_DELIMITER = 2
        NUMERIC_LITERAL = 3
        STRING_LITERAL = 4
        INDENT = 5 # [https://docs.python.org/3/reference/lexical_analysis.html#indentation][-1]
        DEDENT = 6
        STATEMENT_SEPARATOR = 7

    def __init__(self, start, end, category):
        self.start = start
        self.end = end
        self.category = category

def tokenize(source, newline_chars = None, comments = None):
    tokens = []
    indentation_levels = []
    nesting_elements = [] # parentheses, square brackets or curly braces

    begin_of_line = True
    expected_an_indented_block = False
    i = 0

    while i < len(source):
        if begin_of_line: # at the beginning of each line, the line's indentation level is compared to the last of the indentation_levels [:1]
            begin_of_line = False
            linestart = i
            indentation_level = 0
            while i < len(source):
                if source[i] == ' ':
                    indentation_level += 1
                elif source[i] == "\t":
                    indentation_level += 8 # consider tab as just 8 spaces (I know that Python 3 use different rules, but I disagree with Python 3 approach ([-1]:‘Tabs are replaced (from left to right) by one to eight spaces’), so I decided to use this simpler solution)
                else:
                    break
                i += 1
            if i == len(source): # end of source
                break

            if source[i] in "\r\n#": # lines with only whitespace and/or comments do not affect the indentation
                continue

            prev_indentation_level = indentation_levels[-1] if len(indentation_levels) else 0

            if expected_an_indented_block:
                if indentation_level > prev_indentation_level:
                    expected_an_indented_block = False
                else:
                    raise Exception('expected an indented block')

            if indentation_level == prev_indentation_level: # [1:] [-1]:‘If it is equal, nothing happens.’ [:2]
                if len(tokens):
                    tokens.append(Token(linestart-1, linestart, Token.Category.STATEMENT_SEPARATOR))
            elif indentation_level > prev_indentation_level: # [2:] [-1]:‘If it is larger, it is pushed on the stack, and one INDENT token is generated.’ [:3]
                indentation_levels.append(indentation_level)
                tokens.append(Token(i, i, Token.Category.INDENT))
            else: # [3:] [-1]:‘If it is smaller, it ~‘must’ be one of the numbers occurring on the stack; all numbers on the stack that are larger are popped off, and for each number popped off a DEDENT token is generated.’ [:4]
                while True:
                    indentation_levels.pop()
                    tokens.append(Token(i, i, Token.Category.DEDENT))
                    level = indentation_levels[-1] if len(indentation_levels) else 0
                    if level == indentation_level:
                        break
                    if level < indentation_level:
                        raise Exception('unindent does not match any outer indentation level', i)

            prev_indentation_level = indentation_level

        ch = source[i]

        if ch in " \t":
            i += 1 # just skip whitespace characters
        elif ch in "\r\n":
            if newline_chars != None:
                newline_chars.append(i)
            i += 1
            if ch == "\r" and source[i:i+1] == "\n":
                i += 1
            if len(nesting_elements) == 0: # [https://docs.python.org/3/reference/lexical_analysis.html#implicit-line-joining ‘Implicit line joining’]:‘Expressions in parentheses, square brackets or curly braces can be split over more than one physical line without using backslashes.’
                begin_of_line = True
        elif ch == '#':
            i += 1
            comment_start = i
            while i < len(source) and source[i] not in "\r\n":
                i += 1
            if comments != None:
                comments.append((comment_start, i))
        else:
            expected_an_indented_block = ch == ':'

            operator_or_delimiter = None
            for op in operators_and_delimiters:
                if source[i:i+len(op)] == op:
                    operator_or_delimiter = op
                    break

            lexem_start = i
            i += 1

            if operator_or_delimiter:
                i = lexem_start + len(operator_or_delimiter)
                category = Token.Category.OPERATOR_OR_DELIMITER
                if ch in '([{':
                    nesting_elements.append((ch, lexem_start))
                elif ch in ')]}': # ([{
                    if len(nesting_elements) == 0 or nesting_elements[-1][0] != {')':'(', ']':'[', '}':'{'}[ch]: # }])
                        raise Exception('there is no corresponding opening parenthesis/bracket/brace for `' + ch + '`', lexem_start)
                    nesting_elements.pop()
                elif ch == ';':
                    category = Token.Category.STATEMENT_SEPARATOR

            elif 'a' <= ch <= 'z' or 'A' <= ch <= 'Z' or ch == '_': # this is IDENTIFIER or KEYWORD
                while i < len(source):
                    ch = source[i]
                    if not ('a' <= ch <= 'z' or 'A' <= ch <= 'Z' or ch == '_' or '0' <= ch <= '9' or ch == '?'):
                        break
                    i += 1
                if source[lexem_start:i] in keywords:
                    category = Token.Category.KEYWORD
                else:
                    category = Token.Category.IDENTIFIER

            elif '0' <= ch <= '9': # this is NUMERIC_LITERAL
                if ch in '01' and source[i:i+1] == 'B':
                    i += 1
                else:
                    while i < len(source) and '0' <= source[i] <= '9':
                        i += 1
                category = Token.Category.NUMERIC_LITERAL

            elif ch == '"':
                startqpos = i - 1
                while True:
                    if i == len(source):
                        raise Exception('unclosed string literal', startqpos)
                    ch = source[i]
                    i += 1
                    if ch == '"':
                        break
                category = Token.Category.STRING_LITERAL

            else:
                raise Exception('unexpected character ' + ch, lexem_start)

            tokens.append(Token(lexem_start, i, category))

    if len(nesting_elements):
        raise Exception('there is no corresponding closing parenthesis/bracket/brace for `' + nesting_elements[-1][0] + '`', nesting_elements[-1][1])

    while len(indentation_levels): # [4:] [-1]:‘At the end of the file, a DEDENT token is generated for each number remaining on the stack that is larger than zero.’
        tokens.append(Token(i, i, Token.Category.DEDENT))
        indentation_levels.pop()

    return tokens
