try:
    from python_to_11l.tokenizer import Token
    import python_to_11l.tokenizer as tokenizer
except ImportError:
    from tokenizer import Token
    import tokenizer
from typing import List, Tuple, Dict, Callable
from enum import IntEnum
import os, re

class Scope:
    parent : 'Scope'
    class Var:
        type : str
        node : 'ASTNode'
        def __init__(self, type, node):
            self.type = type
            self.node = node
    vars : Dict[str, Var]
    nonlocals : set
    globals   : set
    is_function : bool

    def __init__(self, func_args):
        self.parent = None
        if func_args != None:
            self.is_function = True
            self.vars = dict(map(lambda x: (x, Scope.Var(None, None)), func_args))
        else:
            self.is_function = False
            self.vars = {}
        self.nonlocals = set()
        self.globals   = set()

    def add_var(self, name, error_if_already_defined = False, type = None, err_token = None, node = None):
        s = self
        while True:
            if name in s.nonlocals or name in s.globals:
                return False
            if s.is_function:
                break
            s = s.parent
            if s == None:
                break

        if not (name in self.vars):
            s = self
            while True:
                if name in s.vars:
                    return False
                if s.is_function:
                    break
                s = s.parent
                if s == None:
                    break
            self.vars[name] = Scope.Var(type, node)
            return True
        elif error_if_already_defined:
            raise Error('redefinition of already defined variable is not allowed', err_token if err_token != None else token)
        return False

    def find_and_get_prefix(self, name, token):
        if name == 'self':
            return ''
        if name in ('isinstance', 'len', 'super', 'print', 'input', 'ord', 'chr', 'range', 'zip', 'abs', 'sum', 'open', 'min', 'max', 'hex', 'map', 'list', 'dict', 'sorted', 'filter', 'round', 'enumerate', 'NotImplementedError'):
            return ''

        s = self
        while True:
            if name in s.nonlocals:
                return '@'
            if name in s.globals:
                return ':'
            if s.is_function:
                break
            s = s.parent
            if s == None:
                break

        capture_level = 0
        s = self
        while True:
            if name in s.vars:
                if s.parent == None: # variable is declared in the global scope
                    if s.vars[name].type == '(Module)':
                        return ':::'
                    return ':' if capture_level > 0 else ''
                else:
                    return capture_level*'@'
            if s.is_function:
                capture_level += 1
            s = s.parent
            if s == None:
                raise Error('undefined identifier', token)

    def find(self, name):
        s = self
        while True:
            id = s.vars.get(name)
            if id != None:
                return id
            s = s.parent
            if s == None:
                return None

    def var_type(self, name):
        id = self.find(name)
        return id.type if id != None else None

scope : Scope

class SymbolBase:
    id : str
    lbp : int
    nud_bp : int
    led_bp : int
    nud : Callable[['SymbolNode'], 'SymbolNode']
    led : Callable[['SymbolNode', 'SymbolNode'], 'SymbolNode']

    def set_nud_bp(self, nud_bp, nud):
        self.nud_bp = nud_bp
        self.nud    = nud

    def set_led_bp(self, led_bp, led):
        self.led_bp = led_bp
        self.led    = led

    def __init__(self):
        def nud(s): raise Error('unknown unary operator', s.token)
        self.nud = nud
        def led(s, l): raise Error('unknown binary operator', s.token)
        self.led = led

class SymbolNode:
    token : Token
    symbol : SymbolBase = None
    children : List['SymbolNode']# = []
    parent : 'SymbolNode' = None
    ast_parent : 'ASTNode'
    function_call : bool = False
    tuple   : bool = False
    is_list : bool = False
    slicing : bool = False
    is_not  : bool = False
    scope_prefix : str = ''
    scope : Scope
    token_str_override : str

    def __init__(self, token, token_str_override = None):
        self.token = token
        self.children = []
        self.scope = scope
        self.token_str_override = token_str_override

    def var_type(self):
        if self.symbol.id == '.':
            if self.children[0].token_str() == 'os' and self.children[1].token_str() == 'pathsep':
                return 'str'
            return None
        return self.scope.var_type(self.token.value(source))

    def append_child(self, child):
        child.parent = self
        self.children.append(child)

    def leftmost(self):
        if self.token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL, Token.Category.NAME, Token.Category.CONSTANT) or self.symbol.id == 'lambda':
            return self.token.start

        if self.symbol.id == '(': # )
            if self.function_call:
                return self.children[0].token.start
            else:
                return self.token.start
        elif self.symbol.id == '[': # ]
            if self.is_list:
                return self.token.start
            else:
                return self.children[0].token.start

        if len(self.children) in (2, 3):
            return self.children[0].leftmost()

        return self.token.start

    def rightmost(self):
        if self.token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL, Token.Category.NAME, Token.Category.CONSTANT):
            return self.token.end

        if self.symbol.id in '([': # ])
            return (self.children[-1] or self.children[-2]).rightmost() + 1

        return self.children[-1].rightmost()

    def token_str(self):
        return self.token.value(source) if not self.token_str_override else self.token_str_override

    def to_str(self):
        # r = ''
        # prev_token_end = self.children[0].token.start
        # for c in self.children:
        #     r += source[prev_token_end:c.token.start]
        #     if c.token.value(source) != 'self': # hack for a while
        #         r += c.token.value(source)
        #     prev_token_end = c.token.end
        # return r
        if self.token.category == Token.Category.NAME:
            if self.scope_prefix == ':' and self.parent and self.parent.function_call: # global functions do not require prefix `:` because global functions are ok, but global variables are not so good and they should be marked with `:`
                return self.token.value(source)
            return self.scope_prefix + self.token_str()

        if self.token.category == Token.Category.NUMERIC_LITERAL:
            n = self.token.value(source)
            i = 0
            # if n[0] in '-+':
            #     sign = n[0]
            #     i = 1
            # else:
            #     sign = ''
            sign = ''
            is_hex = n[i:i+1] == '0' and n[i+1:i+2] in ('x', 'X')
            is_oct = n[i:i+1] == '0' and n[i+1:i+2] in ('o', 'O')
            is_bin = n[i:i+1] == '0' and n[i+1:i+2] in ('b', 'B')
            if is_hex or is_oct or is_bin:
                i += 2
                if is_hex:
                    n = n[i:].replace('_', '')
                    if len(n) <= 4: # short hexadecimal number
                        n = '0'*(4-len(n)) + n
                        return n[:2] + "'" + n[2:]
                    else:
                        number_with_separators = ''
                        j = len(n)
                        while j > 4:
                            number_with_separators = "'" + n[j-4:j] + number_with_separators
                            j -= 4
                        return sign + '0'*(4-j) + n[0:j] + number_with_separators
            return sign + n[i:].replace('_', "'") + ('o' if is_oct else 'b' if is_bin else '')

        if self.token.category == Token.Category.STRING_LITERAL:
            def balance_pq_string(s):
                min_nesting_level = 0
                nesting_level = 0
                for ch in s:
                    if ch == "‘":
                        nesting_level += 1
                    elif ch == "’":
                        nesting_level -= 1
                        min_nesting_level = min(min_nesting_level, nesting_level)
                nesting_level -= min_nesting_level
                return "'"*-min_nesting_level + "‘"*-min_nesting_level + "‘" + s + "’" + "’"*nesting_level + "'"*nesting_level

            s = self.token.value(source)
            if s[0] in 'rR':
                l = 3 if s[1:4] in ('"""', "'''") else 1
                return balance_pq_string(s[1+l:-l])
            elif s[0] in 'bB':
                return s[1:] + '.code'
            else:
                l = 3 if s[0:3] in ('"""', "'''") else 1
                if '\\' in s or ('‘' in s and not '’' in s) or (not '‘' in s and '’' in s):
                    s = s.replace("\n", "\\n\\\n").replace("\\\\n\\\n", "\\\n")
                    if s[0] == '"':
                        return s if l == 1 else '"' + s[3:-3].replace('"', R'\"') + '"'
                    else:
                        return '"' + s[l:-l].replace('"', R'\"').replace(R"\'", "'") + '"'
                else:
                    return balance_pq_string(s[l:-l])

        if self.token.category == Token.Category.CONSTANT:
            return {'None': 'N', 'False': '0B', 'True': '1B'}[self.token.value(source)]

        def range_need_space(child1, child2):
            return not((child1 == None or child1.token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL))
                   and (child2 == None or child2.token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL)))

        if self.symbol.id == '(': # )
            if self.function_call:
                if self.children[0].symbol.id == '.':
                    c01 = self.children[0].children[1].token.value(source)
                    if self.children[0].children[0].symbol.id == '{' and c01 == 'get': # } # replace `{'and':'&', 'or':'|', 'in':'C'}.get(self.symbol.id, 'symbol-' + self.symbol.id)` with `(S .symbol.id {‘and’ {‘&’}; ‘or’ {‘|’}; ‘in’ {‘C’} E ‘symbol-’(.symbol.id)})`
                        parenthesis = ('(', ')') if self.parent != None else ('', '')
                        return parenthesis[0] + self.children[0].to_str() + parenthesis[1]
                    if c01 == 'join' and not (self.children[0].children[0].symbol.id == '.' and self.children[0].children[0].children[0].token_str() == 'os'): # replace `', '.join(arr)` with `arr.join(‘, ’)`
                        assert(len(self.children) == 3)
                        return (self.children[1].to_str() if self.children[1].token.category == Token.Category.NAME or self.children[1].symbol.id == 'for' or self.children[1].function_call else '(' + self.children[1].to_str() + ')') + '.join(' + self.children[0].children[0].to_str() + ')'
                    if c01 == 'split' and len(self.children) == 5 and not (self.children[0].children[0].token_str() == 're'): # split() second argument [limit] in 11l is similar to JavaScript, Ruby and PHP, but not Python
                        return self.children[0].to_str() + '(' + self.children[1].to_str() + ', ' + self.children[3].to_str() + ' + 1)'
                    if c01 == 'split' and len(self.children) == 1:
                        return self.children[0].to_str() + '((‘ ’, "\\t", "\\r", "\\n"), group_delimiters\' 1B)'
                    repl = {'startswith':'starts_with', 'endswith':'ends_with', 'find':'findi', 'rfind':'rfindi', 'lower':'lowercase', 'islower':'is_lowercase', 'upper':'uppercase', 'isupper':'is_uppercase', 'isdigit':'is_digit', 'timestamp':'unix_time', 'lstrip':'ltrim', 'rstrip':'rtrim', 'strip':'trim'}.get(c01, '')
                    if repl != '': # replace `startswith` with `starts_with`, `endswith` with `ends_with`, etc.
                        c00 = self.children[0].children[0].to_str()
                        if repl == 'uppercase' and c00.endswith('[2..]') and self.children[0].children[0].children[0].symbol.id == '(' and self.children[0].children[0].children[0].children[0].token_str() == 'hex': # ) # `hex(x)[2:].upper()` -> `hex(x)`
                            return 'hex(' + self.children[0].children[0].children[0].children[1].to_str() + ')'
                        #assert(len(self.children) == 3)
                        res = c00 + '.' + repl + '('
                        def is_char(child):
                            ts = child.token_str()
                            return child.token.category == Token.Category.STRING_LITERAL and (len(ts) == 3 or (ts[:2] == '"\\' and len(ts) == 4))
                        if repl.endswith('trim') and not is_char(self.children[1]): # `"...".strip("\t ")` -> `"...".trim(Array[Char]("\t "))`
                            assert(len(self.children) == 3)
                            res += 'Array[Char](' + self.children[1].to_str() + ')'
                        else:
                            for i in range(1, len(self.children), 2):
                                assert(self.children[i+1] == None)
                                res += self.children[i].to_str()
                                if i < len(self.children)-2:
                                    res += ', '
                        return res + ')'
                    if self.children[0].children[0].symbol.id == '(' and \
                       self.children[0].children[0].children[0].token_str() == 'open' and \
                   len(self.children[0].children[0].children) == 5 and \
                       self.children[0].children[0].children[4] == None and \
                       self.children[0].children[0].children[3].token_str() in ("'rb'", '"rb"') and \
                       self.children[0].children[1].token_str() == 'read': # ) # transform `open(fname, 'rb').read()` into `File(fname).read_bytes()`
                        assert(self.children[0].children[0].children[2] == None)
                        return 'File(' + self.children[0].children[0].children[1].to_str() + ').read_bytes()'
                    if self.children[0].children[1].token.value(source) == 'total_seconds': # `delta.total_seconds()` -> `delta.seconds`
                        assert(len(self.children) == 1)
                        return self.children[0].children[0].to_str() + '.seconds'
                    if self.children[0].children[1].token.value(source) == 'readlines': # `f.readlines()` -> `f.read_lines(1B)`
                        assert(len(self.children) == 1)
                        return self.children[0].children[0].to_str() + ".read_lines(1B)"
                    if self.children[0].children[0].token_str() == 're' and self.children[0].children[1].token_str() != 'compile': # `re.search('pattern', 'string')` -> `re:‘pattern’.search(‘string’)`
                        c1_in_braces_if_needed = self.children[1].to_str()
                        if self.children[1].token.category != Token.Category.STRING_LITERAL:
                            c1_in_braces_if_needed = '(' + c1_in_braces_if_needed + ')'
                        if self.children[0].children[1].token_str() == 'split': # `re.split('pattern', 'string')` -> `‘string’.split(re:‘pattern’)`
                            return self.children[3].to_str() + '.split(re:' + c1_in_braces_if_needed + ')'
                        if self.children[0].children[1].token_str() == 'sub': # `re.sub('pattern', 'repl', 'string')` -> `‘string’.replace(re:‘pattern’, ‘repl’)`
                            return self.children[5].to_str() + '.replace(re:' + c1_in_braces_if_needed + ', ' + re.sub(R'\\(\d{1,2})', R'$\1', self.children[3].to_str()) + ')'
                        if self.children[0].children[1].token_str() == 'match':
                            assert c1_in_braces_if_needed[0] != '(', 'only string literal patterns supported in `match()` for a while' # )
                            if c1_in_braces_if_needed[-2] == '$': # `re.match('pattern$', 'string')` -> `re:‘pattern’.match(‘string’)`
                                return 're:' + c1_in_braces_if_needed[:-2] + c1_in_braces_if_needed[-1] + '.match(' + self.children[3].to_str() + ')'
                            else: # `re.match('pattern', 'string')` -> `re:‘^pattern’.search(‘string’)`
                                return 're:' + c1_in_braces_if_needed[0] + '^' + c1_in_braces_if_needed[1:] + '.search(' + self.children[3].to_str() + ')'
                        c0c1 = self.children[0].children[1].token_str()
                        return 're:' + c1_in_braces_if_needed + '.' + {'fullmatch': 'match', 'findall': 'find_strings', 'finditer': 'find_matches'}.get(c0c1, c0c1) + '(' + self.children[3].to_str() + ')'
                    if self.children[0].children[0].token_str() == 'collections' and self.children[0].children[1].token_str() == 'defaultdict': # `collections.defaultdict(ValueType) # KeyType` -> `DefaultDict[KeyType, ValueType]()`
                        assert(len(self.children) == 3)
                        if source[self.children[1].token.end + 2] != '#':
                            raise Error('to use defaultdict the type of dict keys must be specified in the comment', self.children[0].children[1].token)
                        sl = slice(self.children[1].token.end + 3, source.find("\n", self.children[1].token.end + 3))
                        return 'DefaultDict[' + trans_type(source[sl].lstrip(' '), self.scope, Token(sl.start, sl.stop, Token.Category.NAME)) + ', ' \
                                              + trans_type(self.children[1].to_str(), self.scope, self.children[1].token) + ']()'
                    if self.children[0].children[0].token_str() == 'random' and self.children[0].children[1].token_str() == 'shuffle':
                        return 'random:shuffle(&' + self.children[1].to_str() + ')'

                func_name = self.children[0].to_str()
                if func_name == 'str':
                    func_name = 'String'
                elif func_name == 'int':
                    func_name = 'Int'
                elif func_name == 'float':
                    func_name = 'Float'
                elif func_name == 'list': # `list(map(...))` -> `map(...)`
                    assert(len(self.children) == 3 and self.children[1].symbol.id == '(' and self.children[1].children[0].token_str() == 'map') # )
                    return self.children[1].to_str()
                elif func_name == 'dict':
                    func_name = 'Dict'
                elif func_name == 'open':
                    func_name = 'File'
                    mode = '‘r’'
                    for i in range(1, len(self.children), 2):
                        if self.children[i+1] == None:
                            if i == 3:
                                mode = self.children[i].to_str()
                        else:
                            arg_name = self.children[i].to_str()
                            if arg_name == 'mode':
                                mode = self.children[i+1].to_str()
                            elif arg_name == 'newline':
                                if mode not in ('‘w’', '"w"'):
                                    raise Error("`newline` argument is only supported in 'w' mode", self.children[i].token)
                                if self.children[i+1].to_str() != '"\\n"':
                                    raise Error(R'the only allowed value for `newline` argument is `"\n"`', self.children[i+1].token)
                                self.children.pop(i+1)
                                self.children.pop(i)
                                break

                if func_name == 'len': # replace `len(container)` with `container.len`
                    assert(len(self.children) == 3)
                    if isinstance(self.ast_parent, ASTIf) if self.parent == None else self.parent.symbol.id == 'if':
                        return '!' + self.children[1].to_str() + '.empty'
                    return self.children[1].to_str() + '.len'
                elif func_name == 'ord': # replace `ord(ch)` with `ch.code`
                    assert(len(self.children) == 3)
                    return self.children[1].to_str() + '.code'
                elif func_name == 'chr': # replace `chr(code)` with `Char(code' code)`
                    assert(len(self.children) == 3)
                    return "Char(code' " + self.children[1].to_str() + ')'
                elif func_name == 'isinstance': # replace `isinstance(obj, type)` with `T(obj) >= type`
                    assert(len(self.children) == 5)
                    return 'T(' + self.children[1].to_str() + ') >= ' + self.children[3].to_str()
                elif func_name in ('map', 'filter'): # replace `map(function, iterable)` with `iterable.map(function)`
                    assert(len(self.children) == 5)
                    b = self.children[3].symbol.id == 'if'
                    return '('*b + self.children[3].to_str() + ')'*b + '.' + func_name + '(' + self.children[1].to_str() + ')'
                elif func_name == 'super': # replace `super()` with `T.base`
                    assert(len(self.children) == 1)
                    return 'T.base'
                elif func_name == 'range':
                    assert(3 <= len(self.children) <= 7)
                    parenthesis = ('(', ')') if self.parent != None else ('', '')
                    if len(self.children) == 3: # replace `range(e)` with `(0 .< e)`
                        space = ' ' * range_need_space(self.children[1], None)
                        return parenthesis[0] + '0' + space + '.<' + space + self.children[1].to_str() + parenthesis[1]
                    else:
                        rangestr = ' .< ' if range_need_space(self.children[1], self.children[3]) else '.<'
                        if len(self.children) == 5: # replace `range(b, e)` with `(b .< e)`
                            return parenthesis[0] + self.children[1].to_str() + rangestr + self.children[3].to_str() + parenthesis[1]
                        else: # replace `range(b, e, step)` with `(b .< e).step(step)`
                            return '(' + self.children[1].to_str() + rangestr + self.children[3].to_str() + ').step(' + self.children[5].to_str() + ')'
                else:
                    res = func_name + '('
                    for i in range(1, len(self.children), 2):
                        if self.children[i+1] == None:
                            res += self.children[i].to_str()
                        else:
                            res += self.children[i].to_str() + "' " + self.children[i+1].to_str()
                        if i < len(self.children)-2:
                            res += ', '
                    return res + ')'
            elif self.tuple:
                res = '('
                for i in range(len(self.children)):
                    res += self.children[i].to_str()
                    if i < len(self.children)-1:
                        res += ', '
                if len(self.children) == 1:
                    res += ','
                return res + ')'
            else:
                assert(len(self.children) == 1)
                return '(' + self.children[0].to_str() + ')'

        elif self.symbol.id == '[': # ]
            if self.is_list:
                if len(self.children) == 1 and self.children[0].symbol.id == 'for':
                    return self.children[0].to_str()
                res = '['
                for i in range(len(self.children)):
                    res += self.children[i].to_str()
                    if i < len(self.children)-1:
                        res += ', '
                return res + ']'
            elif self.children[0].symbol.id == '{': # }
                parenthesis = ('(', ')') if self.parent != None else ('', '')
                res = parenthesis[0] + 'S ' + self.children[1].to_str() + ' {'
                for i in range(0, len(self.children[0].children), 2):
                    res += self.children[0].children[i].to_str() + ' {' + self.children[0].children[i+1].to_str() + '}'
                    if i < len(self.children[0].children)-2:
                        res += '; '
                return res + '}' + parenthesis[1]
            else:
                c0 = self.children[0].to_str()
                if self.slicing:
                    def for_negative_bound(c):
                        child = self.children[c]
                        if child == None:
                            return None
                        r = child.to_str()
                        if r[0] == '-': # hacky implementation of ‘this rule’[https://docs.python.org/3/reference/simple_stmts.html]:‘If either bound is negative, the sequence's length is added to it.’
                            r = '(len)' + r
                        return r
                    space = ' ' * range_need_space(self.children[1], self.children[2])
                    fnb2 = for_negative_bound(2)
                    s = (for_negative_bound(1) or '0') + space + '.' + ('<' + space + fnb2 if fnb2 else '.')
                    if len(self.children) == 4 and self.children[3] != None:
                        s = '(' + s + ').step(' + self.children[3].to_str() + ')'
                    return c0 + '[' + s + ']'
                elif self.children[1].to_str() == '-1':
                    return c0 + '.last'
                else:
                    return (c0 + '['
                        + '(len)'*(self.children[1].symbol.id == '-' and len(self.children[1].children) == 1) # hacky implementation of ‘this rule’[https://docs.python.org/3/reference/simple_stmts.html]:‘the subscript must yield an integer. If it is negative, the sequence's length is added to it.’
                        + self.children[1].to_str() + ']')

        elif self.symbol.id == '{': # }
            if len(self.children) == 0:
                return 'Dict()'
            res = '['
            for i in range(0, len(self.children), 2):
                res += self.children[i].to_str() + ' = ' + self.children[i+1].to_str()
                if i < len(self.children)-2:
                    res += ', '
            return res + ']'

        elif self.symbol.id == 'lambda':
            r = '(' if len(self.children) != 3 else ''
            for i in range(0, len(self.children)-1, 2):
                r += self.children[i].to_str()
                if self.children[i+1] != None:
                    r += ' = ' + self.children[i+1].to_str()
                if i < len(self.children)-3:
                    r += ', '
            if len(self.children) != 3: r += ')'
            return r + ' -> ' + self.children[-1].to_str()

        elif self.symbol.id == 'for':
            res = self.children[2].children[0].children[0].to_str() if self.children[2].symbol.id == '(' and len(self.children[2].children) == 1 and self.children[2].children[0].symbol.id == '.' and len(self.children[2].children[0].children) == 2 and self.children[2].children[0].children[1].token_str() == 'items' else self.children[2].to_str() # )
            if len(self.children) == 4:
                res += '.filter(' + self.children[1].to_str() + ' -> ' + self.children[3].to_str() + ')'
            if self.children[1].to_str() != self.children[0].to_str():
                res += '.map(' + self.children[1].to_str() + ' -> ' + self.children[0].to_str() + ')'
            return res

        elif self.symbol.id == 'not':
            if len(self.children) == 1:
                if (self.children[0].token.category == Token.Category.OPERATOR_OR_DELIMITER or (self.children[0].token.category == Token.Category.KEYWORD and self.children[0].symbol.id == 'in')) and len(self.children[0].children) == 2:
                    return '!(' + self.children[0].to_str() + ')'
                else:
                    return '!' + self.children[0].to_str()
            else:
                assert(len(self.children) == 2)
                return self.children[0].to_str() + ' !C ' + self.children[1].to_str()

        elif self.symbol.id == 'is':
            return '&' + self.children[0].to_str() + (' != ' if self.is_not else ' == ') + '&' + self.children[1].to_str()

        if len(self.children) == 1:
            #return '(' + self.symbol.id + self.children[0].to_str() + ')'
            return {'~':'(-)'}.get(self.symbol.id, self.symbol.id) + self.children[0].to_str()
        elif len(self.children) == 2:
            #return '(' + self.children[0].to_str() + ' ' + self.symbol.id + ' ' + self.children[1].to_str() + ')'
            if self.symbol.id == '.':
                if self.children[0].symbol.id == '{' and self.children[1].token.category == Token.Category.NAME and self.children[1].token.value(source) == 'get': # } # replace `{'and':'&', 'or':'|', 'in':'C'}.get(self.symbol.id, 'symbol-' + self.symbol.id)` with `(S .symbol.id {‘and’ {‘&’}; ‘or’ {‘|’}; ‘in’ {‘C’} E ‘symbol-’(.symbol.id)})`
                    res = 'S ' + self.parent.children[1].to_str() + ' {'
                    for i in range(0, len(self.children[0].children), 2):
                        res += self.children[0].children[i].to_str() + ' {' + self.children[0].children[i+1].to_str() + '}'
                        if i < len(self.children[0].children)-2:
                            res += '; '
                    return res + ' E ' + self.parent.children[3].to_str() + '}'

                c1ts = self.children[1].token_str()
                if self.children[0].token_str() == 'sys' and c1ts in ('argv', 'exit', 'stdin', 'stdout', 'stderr'):
                    return ':'*(c1ts != 'exit') + c1ts

                if self.children[0].scope_prefix == ':::':
                    if self.children[0].token_str() == 'math':
                        c1 = self.children[1].to_str()
                        if c1 == 'fabs': c1 = 'abs'
                        return c1
                    r = self.children[0].token_str() + ':' + self.children[1].to_str()
                    return {'tempfile:gettempdir': 'fs:get_temp_dir', 'os:path': 'fs:path', 'os:pathsep': 'os:env_path_sep', 'os:sep': 'fs:path:sep', 'os:system': 'os:', 'os:listdir': 'fs:list_dir', 'os:walk': 'fs:walk_dir',
                    'os:mkdir': 'fs:create_directory', 'os:makedirs': 'fs:create_directories', 'os:remove': 'fs:remove', 'os:rename': 'fs:rename',
                    'time:time': 'time:().unix_time', 'time:sleep': 'sleep', 'datetime:datetime': 'time:', 'datetime:date': 'time:', 'datetime:timedelta': 'time:delta', 're:compile': 're:',
                    'random:randrange': 'random:'}.get(r, r)

                if self.children[0].symbol.id == '.' and self.children[0].children[0].scope_prefix == ':::':
                    if self.children[0].children[0].token_str() == 'datetime':
                        if self.children[0].children[1].token_str() == 'datetime':
                            if self.children[1].token_str() == 'now': # `datetime.datetime.now()` -> `time:()`
                                return 'time:'
                            if self.children[1].token_str() == 'fromtimestamp': # `datetime.datetime.fromtimestamp()` -> `time:from_unix_time()`
                                return 'time:from_unix_time'
                            if self.children[1].token_str() == 'strptime': # `datetime.datetime.strptime()` -> `time:strptime()`
                                return 'time:strptime'
                        if self.children[0].children[1].token_str() == 'date' and self.children[1].token_str() == 'today': # `datetime.date.today()` -> `time:today()`
                            return 'time:today'
                    if self.children[0].children[0].token_str() == 'os' and self.children[0].children[1].token_str() == 'path':
                        r = {'pathsep':'os:env_path_sep', 'isdir':'fs:is_directory', 'isfile':'fs:is_file', 'islink':'fs:is_symlink',
                             'dirname':'fs:path:dir_name', 'basename':'fs:path:base_name', 'abspath':'fs:path:absolute', 'relpath':'fs:path:relative',
                             'getsize':'fs:file_size', 'splitext':'fs:path:split_ext'}.get(self.children[1].token_str(), '')
                        if r != '':
                            return r

                if len(self.children[0].children) == 2 and self.children[0].children[0].scope_prefix == ':::' and self.children[0].children[0].token_str() != 'sys': # for `os.path.join()` [and also take into account `sys.argv.index()`]
                    return self.children[0].to_str() + ':' + self.children[1].to_str()

                if self.children[0].to_str() == 'self':
                    parent = self
                    while parent.parent:
                        parent = parent.parent
                    ast_parent = parent.ast_parent
                    function_nesting = 0
                    while type(ast_parent) != ASTProgram:
                        if type(ast_parent) == ASTFunctionDefinition:
                            if len(ast_parent.function_arguments) >= 1 and ast_parent.function_arguments[0][0] == 'self' and type(ast_parent.parent) != ASTClassDefinition:
                                return 'self.' + self.children[1].to_str()
                            function_nesting += 1
                            if function_nesting == 2:
                                break
                        elif type(ast_parent) == ASTClassDefinition:
                            break
                        ast_parent = ast_parent.parent
                    return ('@' if function_nesting == 2 else '') + '.' + self.children[1].to_str()

                if c1ts == 'days':
                    return self.children[0].to_str() + '.' + c1ts + '()'

                return self.children[0].to_str() + '.' + self.children[1].to_str()

            elif self.symbol.id == '+=' and self.children[1].symbol.id == '[' and self.children[1].is_list: # ]
                return self.children[0].to_str() + ' [+]= ' + (self.children[1].to_str()[1:-1] if len(self.children[1].children) == 1 else self.children[1].to_str())
            elif self.symbol.id == '+=' and self.children[1].token.value(source) == '1':
                return self.children[0].to_str() + '++'
            elif self.symbol.id == '-=' and self.children[1].token.value(source) == '1':
                return '--' + self.children[0].to_str() if self.parent else self.children[0].to_str() + '--'
            elif self.symbol.id == '+=' and self.children[0].token.category == Token.Category.NAME and self.children[0].var_type() == 'str':
                return self.children[0].to_str() + ' ‘’= ' + self.children[1].to_str()
            elif self.symbol.id == '+=' and self.children[0].token.category == Token.Category.NAME and self.children[0].var_type() == 'List':
                return self.children[0].to_str() + ' [+]= ' + self.children[1].to_str()
            elif self.symbol.id == '+' and self.children[1].symbol.id == '*' and self.children[0].token.category == Token.Category.STRING_LITERAL \
                                                                             and self.children[1].children[1].token.category == Token.Category.STRING_LITERAL: # for `outfile.write('<blockquote'+(ch=='<')*' class="re"'+'>')`
                return self.children[0].to_str() + '(' + self.children[1].to_str() + ')'
            elif self.symbol.id == '+' and self.children[1].symbol.id == '*' and self.children[1].children[0].token.category == Token.Category.STRING_LITERAL \
                                                                             and (self.children[0].token.category == Token.Category.STRING_LITERAL
                                                                              or (self.children[0].symbol.id == '+'
                                                                              and self.children[0].children[1].token.category == Token.Category.STRING_LITERAL)): # for `outfile.write("<table"+' style="display: inline"'*(prevci != 0 and instr[prevci-1] != "\n")+...)` and `outfile.write('<pre>' + ins + '</pre>' + "\n"*(not self.habr_html))`
                return self.children[0].to_str() + '(' + self.children[1].to_str() + ')'
            elif self.symbol.id == '+' and self.children[1].token.category == Token.Category.STRING_LITERAL and ((self.children[0].symbol.id == '+'
                                       and self.children[0].children[1].token.category == Token.Category.STRING_LITERAL) # for `outfile.write(... + '<br /></span>' # ... \n + '<div class="spoiler_text" ...')`
                                        or self.children[0].token.category == Token.Category.STRING_LITERAL): # for `pre {margin: 0;}''' + # ... \n '''...`
                c0 = self.children[0].to_str()
                c1 = self.children[1].to_str()
                return c0 + {('"','"'):'‘’', ('"','‘'):'', ('’','‘'):'""', ('’','"'):''}[(c0[-1], c1[0])] + c1
            elif self.symbol.id == '+' and (self.children[0].token.category == Token.Category.STRING_LITERAL
                                         or self.children[1].token.category == Token.Category.STRING_LITERAL
                                         or (self.children[0].symbol.id == '+' and self.children[0].children[1].token.category == Token.Category.STRING_LITERAL)):
                c1 = self.children[1].to_str()
                return self.children[0].to_str() + ('(' + c1 + ')' if c1[0] == '.' else c1)
            elif self.symbol.id == '+' and self.children[1].symbol.id == '*' and (self.children[1].children[0].token.category == Token.Category.STRING_LITERAL   # for `self.newlines() + ' ' * (indent*3) + 'F ' + ...`
                                                                               or self.children[1].children[1].token.category == Token.Category.STRING_LITERAL): # for `(... + self.ohd*'</span>')`
                p = self.children[0].symbol.id == '*'
                return '('*p + self.children[0].to_str() + ')'*p + '‘’(' + self.children[1].to_str() + ')'
            elif self.symbol.id == '+' and self.children[0].symbol.id == '*' and self.children[0].children[0].token.category == Token.Category.STRING_LITERAL: # for `' ' * (indent*3) + self.expression.to_str() + "\n"`
                c1 = self.children[1].to_str()
                return '(' + self.children[0].to_str() + ')‘’' + ('(' + c1 + ')' if c1[0] == '.' else c1)
            elif self.symbol.id == '+' and (self.children[0].var_type() == 'str' or self.children[1].var_type() == 'str'):
                return self.children[0].to_str() + '‘’' + self.children[1].to_str()
            elif self.symbol.id == '+' and (self.children[0].is_list or self.children[1].is_list):
                return self.children[0].to_str() + ' [+] ' + self.children[1].to_str()
            elif self.symbol.id == '<=' and self.children[0].symbol.id == '<=': # replace `'0' <= ch <= '9'` with `ch C ‘0’..‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' .. ' if range_need_space(self.children[0].children[0], self.children[1]) else '..') + self.children[1].to_str()
            elif self.symbol.id == '<'  and self.children[0].symbol.id == '<=': # replace `'0' <= ch <  '9'` with `ch C ‘0’.<‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' .< ' if range_need_space(self.children[0].children[0], self.children[1]) else '.<') + self.children[1].to_str()
            elif self.symbol.id == '<=' and self.children[0].symbol.id == '<' : # replace `'0' <  ch <= '9'` with `ch C ‘0’<.‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' <. ' if range_need_space(self.children[0].children[0], self.children[1]) else '<.') + self.children[1].to_str()
            elif self.symbol.id == '<'  and self.children[0].symbol.id == '<' : # replace `'0' <= ch <= '9'` with `ch C ‘0’<.<‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' <.< ' if range_need_space(self.children[0].children[0], self.children[1]) else '<.<') + self.children[1].to_str()
            elif self.symbol.id == '==' and self.children[0].symbol.id == '(' and self.children[0].children[0].to_str() == 'len' and self.children[1].token.value(source) == '0': # )
                return self.children[0].children[1].to_str() + '.empty'
            elif self.symbol.id == '%' and self.children[0].token.category == Token.Category.STRING_LITERAL:
                assert(self.children[1].symbol.id == '(')#self.children[1].tuple)
                fmtstr = self.children[0].to_str()
                nfmtstr = ''
                i = 0
                while i < len(fmtstr):
                    fmtchr = fmtstr[i+1:i+2]
                    if fmtstr[i] == '%':
                        if fmtchr == '%':
                            nfmtstr += '%'
                            i += 2
                        elif fmtchr == 'g':
                            nfmtstr += '#.'
                            i += 2
                        else:
                            nfmtstr += '#'
                            before_period = 0
                            after_period = 6
                            i += 1
                            while i < len(fmtstr) and fmtstr[i].isdigit():
                                before_period = before_period*10 + ord(fmtstr[i]) - ord('0')
                                i += 1
                            if fmtstr[i:i+1] == '.':
                                i += 1
                                after_period = 0
                                while i < len(fmtstr) and fmtstr[i].isdigit():
                                    after_period = after_period*10 + ord(fmtstr[i]) - ord('0')
                                    i += 1
                            if fmtstr[i:i+1] == 'd':
                                if before_period != 0:
                                    nfmtstr += str(before_period)
                                nfmtstr += '.0'
                            elif fmtstr[i:i+1] == 's':
                                if before_period != 0:
                                    nfmtstr += str(before_period)
                            elif fmtstr[i:i+1] == 'f':
                                if before_period != 0:
                                    nfmtstr += str(before_period - after_period - 1)
                                nfmtstr += '.' + str(after_period)
                            else:
                                raise ValueError('unsupported format character `' + fmtstr[i:i+1] + '`')
                            i += 1
                        continue

                    nfmtstr += fmtstr[i]
                    i += 1
                return nfmtstr + '.format' + self.children[1].to_str()
            else:
                return self.children[0].to_str() + ' ' + {'and':'&', 'or':'|', 'in':'C', '//':'I/', '//=':'I/=', '**':'^', '^':'(+)'}.get(self.symbol.id, self.symbol.id) + ' ' + self.children[1].to_str()
        elif len(self.children) == 3:
            assert(self.symbol.id == 'if')
            c0 = self.children[0].to_str()
            if self.children[1].symbol.id == '!=' and self.children[1].children[1].token.value(source) == 'None' and self.children[1].children[0].to_str() == c0: # replace `a if a != None else b` with `a ? b`
                return c0 + ' ? ' + self.children[2].to_str()
            return 'I ' + self.children[1].to_str() + ' {' + c0 + '} E ' + self.children[2].to_str()

        return ''

symbol_table : Dict[str, SymbolBase] = {}
allowed_keywords_in_expressions : List[str] = []

def symbol(id, bp = 0):
    try:
        s = symbol_table[id]
    except KeyError:
        s = SymbolBase()
        s.id = id
        s.lbp = bp
        symbol_table[id] = s
        if id[0].isalpha(): # this is keyword-in-expression
            assert(id.isalpha())
            allowed_keywords_in_expressions.append(id)
    else:
        s.lbp = max(bp, s.lbp)
    return s

class ASTNode:
    parent : 'ASTNode'

    def walk_expressions(self, f):
        pass
    def walk_children(self, f):
        pass

class ASTNodeWithChildren(ASTNode):
    # children : List['ASTNode'] = [] # OMFG! This actually means static (common for all objects of type ASTNode) variable, not default value of member variable, that was unexpected to me as it contradicts C++11 behavior
    children : List['ASTNode']
    tokeni : int

    def __init__(self):
        self.children = []
        self.tokeni = tokeni

    def walk_children(self, f):
        for child in self.children:
            f(child)

    def children_to_str(self, indent, t):
        r = ''
        if self.tokeni > 0:
            ti = self.tokeni - 1
            while ti > 0 and tokens[ti].category in (Token.Category.DEDENT, Token.Category.STATEMENT_SEPARATOR):
                ti -= 1
            r = (min(source[tokens[ti].end:tokens[self.tokeni].start].count("\n"), 2) - 1) * "\n"
        r += ' ' * (indent*3) + t + "\n"
        for c in self.children:
            r += c.to_str(indent+1)
        return r

class ASTNodeWithExpression(ASTNode):
    expression : SymbolNode

    def set_expression(self, expression):
        self.expression = expression
        self.expression.ast_parent = self

    def walk_expressions(self, f):
        f(self.expression)

class ASTProgram(ASTNodeWithChildren):
    def to_str(self):
        r = ''
        for c in self.children:
            r += c.to_str(0)
        return r

class ASTImport(ASTNode):
    def __init__(self):
        self.modules = []

    def to_str(self, indent):
        return ' ' * (indent*3) + '//import ' + ', '.join(self.modules) + "\n" # this is easier than avoid to add empty line here: `import sys\n\ndef f()` -> `\nF f()`

class ASTExpression(ASTNodeWithExpression):
    def to_str(self, indent):
        return ' ' * (indent*3) + self.expression.to_str() + "\n"

class ASTExprAssignment(ASTNodeWithExpression):
    add_var : bool = False
    dest_expression : SymbolNode

    def set_dest_expression(self, dest_expression):
        self.dest_expression = dest_expression
        self.dest_expression.ast_parent = self

    def to_str(self, indent):
        return ' ' * (indent*3) + ('V ' if self.add_var and type(self.parent) != ASTClassDefinition else '') + self.dest_expression.to_str() + ' = ' + self.expression.to_str() + "\n"

    def walk_expressions(self, f):
        f(self.dest_expression)
        super().walk_expressions(f)

class ASTAssert(ASTNodeWithExpression):
    expression2 : SymbolNode = None

    def set_expression2(self, expression2):
        self.expression2 = expression2
        self.expression2.ast_parent = self

    def to_str(self, indent):
        return ' ' * (indent*3) + 'assert(' + (self.expression.children[0].to_str() if self.expression.symbol.id == '(' and not self.expression.tuple and not self.expression.function_call # )
            else self.expression.to_str()) + (', ' + self.expression2.to_str() if self.expression2 != None else '') + ")\n"

    def walk_expressions(self, f):
        if self.expression2 != None: f(self.expression2)
        super().walk_expressions(f)

python_types_to_11l = {'&':'&', 'int':'Int', 'float':'Float', 'str':'String', 'bool':'Bool', 'None':'N', 'List':'', 'Tuple':'Tuple', 'Dict':'Dict', 'DefaultDict':'DefaultDict', 'IO[str]': 'File', 'List[List[str]]':'Array[Array[String]]', 'List[str]':'[String]'}

def trans_type(ty, scope, type_token):
    if ty[0] in '\'"':
        assert(ty[-1] == ty[0])
        ty = ty[1:-1]
    t = python_types_to_11l.get(ty)
    if t != None:
        return t
    else:
        p = ty.find('[') # ]
        if p != -1:
            return trans_type(ty[:p], scope, type_token) + '[' + trans_type(ty[p+1:-1], scope, type_token) + ']'

        id = scope.find(ty)
        if id == None:
            raise Error('class `' + ty + '` is not defined', type_token)
        if id.type != '(Class)':
            raise Error('`' + ty + '`: expected a class name (got variable' + (' of type `' + id.type + '`' if id.type != None else '') + ')', type_token)
        return ty

class ASTTypeHint(ASTNode):
    var : str
    type : str
    type_args : List[str]
    scope : Scope
    type_token : Token
    is_reference = False

    def __init__(self):
        self.scope = scope

    def trans_type(self, ty):
        return trans_type(ty, self.scope, self.type_token)

    def to_str_(self, indent, nullable = False):
        if self.type == 'Callable':
            return ' ' * (indent*3) + '(' + ', '.join(self.trans_type(ty) for ty in self.type_args[0].split(',')) + ' -> ' + self.trans_type(self.type_args[1]) + ') ' + self.var
        elif self.type == 'Optional':
            assert(len(self.type_args) == 1)
            return ' ' * (indent*3) + self.trans_type(self.type_args[0]) + '? ' + self.var
        return ' ' * (indent*3) + self.trans_type(self.type) + ('[' + ', '.join(self.trans_type(ty) for ty in self.type_args) + ']' if len(self.type_args) else '') + '?'*nullable + '&'*self.is_reference + ' ' + self.var

    def to_str(self, indent):
        return self.to_str_(indent) + "\n"

class ASTAssignmentWithTypeHint(ASTTypeHint, ASTNodeWithExpression):
    def to_str(self, indent):
        if self.type == 'DefaultDict':
            assert(self.expression.function_call and self.expression.children[0].to_str() == 'collections:defaultdict')
            return super().to_str(indent)

        expression_str = self.expression.to_str()
        if expression_str == 'N':
            return super().to_str_(indent, True) + "\n"
        return super().to_str_(indent) + (' = ' + expression_str if expression_str not in ('[]', 'Dict()') else '') + "\n"

class ASTFunctionDefinition(ASTNodeWithChildren):
    function_name : str
    function_return_type : str = ''
    function_arguments : List[Tuple[str, SymbolNode, str]]# = []
    first_named_only_argument = None
    class VirtualCategory(IntEnum):
        NO = 0
        NEW = 1
        OVERRIDE = 2
        ABSTRACT = 3
    virtual_category = VirtualCategory.NO
    scope : Scope

    def __init__(self):
        super().__init__()
        self.function_arguments = []
        self.scope = scope

    def to_str(self, indent):
        fargs = []
        for arg in self.function_arguments:
            farg = ''
            default_value = ''
            if arg[1] != None:
                default_value = arg[1].to_str()
            if arg[2] != '':
                ty = trans_type(arg[2], self.scope, tokens[self.tokeni])
                farg += ty
                if default_value == 'N':
                    farg += '?'
                farg += ' '
                if ty.startswith(('Array[', '[')): # ]]
                    farg += '&'
            farg += arg[0] + ('' if default_value == '' else ' = ' + default_value)
            fargs.append(farg)
        if self.first_named_only_argument != None:
            fargs.insert(self.first_named_only_argument, "'")
        if len(self.function_arguments) and self.function_arguments[0][0] == 'self' and type(self.parent) == ASTClassDefinition:
            fargs.pop(0)

        if self.virtual_category == self.VirtualCategory.ABSTRACT:
            return ' ' * (indent*3) + 'F.virtual.abstract ' + self.function_name + '(' + ", ".join(fargs) + ') -> ' + python_types_to_11l[self.function_return_type] + "\n"

        return self.children_to_str(indent, ('F', 'F.virtual.new', 'F.virtual.override')[self.virtual_category] + ' ' + (self.function_name if self.function_name != '__init__' else '')
            + '(' + ", ".join(fargs) + ')'
            + ('' if self.function_return_type == '' else ' -> ' + trans_type(self.function_return_type, self.scope, tokens[self.tokeni])))

class ASTIf(ASTNodeWithChildren, ASTNodeWithExpression):
    else_or_elif : ASTNode = None

    def walk_children(self, f):
        super().walk_children(f)
        if self.else_or_elif != None:
            self.else_or_elif.walk_children(f)

    def to_str(self, indent):
        return self.children_to_str(indent, 'I ' + self.expression.to_str()) + (self.else_or_elif.to_str(indent) if self.else_or_elif != None else '')

class ASTElse(ASTNodeWithChildren):
    def to_str(self, indent):
        return self.children_to_str(indent, 'E')

class ASTElseIf(ASTNodeWithChildren, ASTNodeWithExpression):
    else_or_elif : ASTNode = None

    def walk_children(self, f):
        super().walk_children(f)
        if self.else_or_elif != None:
            self.else_or_elif.walk_children(f)

    def to_str(self, indent):
        return self.children_to_str(indent, 'E I ' + self.expression.to_str()) + (self.else_or_elif.to_str(indent) if self.else_or_elif != None else '')

class ASTSwitch(ASTNodeWithExpression):
    class Case(ASTNodeWithChildren, ASTNodeWithExpression):
        def __init__(self):
            super().__init__()
            self.tokeni = 0
    cases : List[Case]

    def __init__(self):
        self.cases = []

    def walk_children(self, f):
        for case in self.cases:
            f(case)

    def to_str(self, indent):
        r = ' ' * (indent*3) + 'S ' + self.expression.to_str() + "\n"
        for case in self.cases:
            r += case.children_to_str(indent + 1, 'E' if case.expression.token_str() == 'E' else case.expression.to_str())
        return r

class ASTWhile(ASTNodeWithChildren, ASTNodeWithExpression):
    def to_str(self, indent):
        return self.children_to_str(indent, 'L' if self.expression.token.category == Token.Category.CONSTANT and self.expression.token.value(source) == 'True' else 'L ' + self.expression.to_str())

class ASTFor(ASTNodeWithChildren, ASTNodeWithExpression):
    was_no_break : ASTNodeWithChildren = None
    loop_variables : List[str]
    os_walk = False
    dir_filter = None

    def walk_children(self, f):
        super().walk_children(f)
        if self.was_no_break != None:
            self.was_no_break.walk_children(f)

    def to_str(self, indent):
        if self.os_walk:
            dir_filter = ''
            if self.dir_filter != None:
                dir_filter = ", dir_filter' " + self.dir_filter # (
            return self.children_to_str(indent, 'L(_fname) ' + self.expression.to_str()[:-1] + dir_filter + ", files_only' 0B)\n"
                + ' ' * ((indent+1)*3) + 'V ' + self.loop_variables[0] + " = fs:path:dir_name(_fname)\n"
                + ' ' * ((indent+1)*3) + '[String] ' + self.loop_variables[1] + ', ' + self.loop_variables[2] + "\n"
                + ' ' * ((indent+1)*3) + 'I fs:is_directory(_fname) {' + self.loop_variables[1] + ' [+]= fs:path:base_name(_fname)} E ' + self.loop_variables[2] + ' [+]= fs:path:base_name(_fname)')

        if len(self.loop_variables) == 1:
            r = 'L(' + self.loop_variables[0] + ') ' + self.expression.to_str()
        elif self.expression.symbol.id == '(' and len(self.expression.children) == 1 and self.expression.children[0].symbol.id == '.' and len(self.expression.children[0].children) == 2 and self.expression.children[0].children[1].token_str() == 'items': # )
            r = 'L(' + ', '.join(self.loop_variables) + ') ' + self.expression.children[0].children[0].to_str()
        else:
            r = 'L(' + ''.join(self.loop_variables) + ') ' + self.expression.to_str()
            for index, loop_var in enumerate(self.loop_variables):
                r += "\n" + ' ' * ((indent+1)*3) + 'V ' + loop_var + ' = ' + ''.join(self.loop_variables) + '[' + str(index) + ']'
        r = self.children_to_str(indent, r)

        if self.was_no_break != None:
            r += self.was_no_break.children_to_str(indent + 1, 'L.was_no_break')

        return r

class ASTContinue(ASTNode):
    def to_str(self, indent):
        return ' ' * (indent*3) + "L.continue\n"

class ASTBreak(ASTNode):
    def to_str(self, indent):
        return ' ' * (indent*3) + "L.break\n"

class ASTReturn(ASTNodeWithExpression):
    def to_str(self, indent):
        return ' ' * (indent*3) + 'R' + (' ' + self.expression.to_str() if self.expression != None else '') + "\n"

    def walk_expressions(self, f):
        if self.expression != None: f(self.expression)

class ASTException(ASTNodeWithExpression):
    def to_str(self, indent):
        return ' ' * (indent*3) + 'X ' + self.expression.to_str() + "\n"

class ASTExceptionTry(ASTNodeWithChildren):
    def to_str(self, indent):
        return self.children_to_str(indent, 'X.try')

class ASTExceptionCatch(ASTNodeWithChildren):
    exception_object_type : str
    exception_object_name : str = ''

    def to_str(self, indent):
        return self.children_to_str(indent, 'X.catch ' + self.exception_object_type + (' ' + self.exception_object_name if self.exception_object_name != '' else ''))

class ASTClassDefinition(ASTNodeWithChildren):
    base_class_name : str = None
    base_class_node : 'ASTClassDefinition' = None
    class_name : str

    def to_str(self, indent):
        return self.children_to_str(indent, 'T ' + self.class_name + ('(' + self.base_class_name + ')' if self.base_class_name and self.base_class_name != 'Exception' else ''))

class ASTPass(ASTNode):
    def to_str(self, indent):
        return ' ' * ((indent-1)*3) + "{\n"\
             + ' ' * ((indent-1)*3) + "}\n"

class ASTStart(ASTNodeWithChildren):
    def to_str(self, indent):
        return self.children_to_str(indent-1, ':start:')

class Error(Exception):
    def __init__(self, message, token):
        self.message = message
        self.pos = token.start
        self.end = token.end

def next_token():
    global token, tokeni, tokensn
    if token == None and tokeni != -1:
        raise Error('no more tokens', Token(len(source), len(source), Token.Category.STATEMENT_SEPARATOR))
    tokeni += 1
    if tokeni == len(tokens):
        token = None
        tokensn = None
    else:
        token = tokens[tokeni]
        tokensn = SymbolNode(token)
        if token.category != Token.Category.INDENT:
            if token.category != Token.Category.KEYWORD or token.value(source) in allowed_keywords_in_expressions:
                key : str
                if token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL):
                    key = '(literal)'
                elif token.category == Token.Category.NAME:
                    key = '(name)'
                elif token.category == Token.Category.CONSTANT:
                    key = '(constant)'
                elif token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT):
                    key = ';'
                else:
                    key = token.value(source)
                tokensn.symbol = symbol_table[key]

def advance(value):
    if token.value(source) != value:
        raise Error('expected `' + value + '`', token)
    next_token()

def peek_token(how_much = 1):
    return tokens[tokeni+how_much] if tokeni+how_much < len(tokens) else Token()

# This implementation is based on [http://svn.effbot.org/public/stuff/sandbox/topdown/tdop-4.py]
def expression(rbp = 0):
    def check_tokensn():
        if tokensn.symbol == None:
            raise Error('no symbol corresponding to token `' + token.value(source) + '` (belonging to ' + str(token.category) +') found while parsing expression', token)
    check_tokensn()
    t = tokensn
    next_token()
    check_tokensn()
    left = t.symbol.nud(t)
    while rbp < tokensn.symbol.lbp:
        t = tokensn
        next_token()
        left = t.symbol.led(t, left)
        check_tokensn()
    return left

def infix(id, bp):
    def led(self, left):
        self.append_child(left)
        self.append_child(expression(self.symbol.led_bp))
        return self
    symbol(id, bp).set_led_bp(bp, led)

def infix_r(id, bp):
    def led(self, left):
        self.append_child(left)
        self.append_child(expression(self.symbol.led_bp - 1))
        return self
    symbol(id, bp).set_led_bp(bp, led)

def prefix(id, bp):
    def nud(self):
        self.append_child(expression(self.symbol.nud_bp))
        return self
    symbol(id).set_nud_bp(bp, nud)

symbol("lambda", 20)
symbol("if", 20); symbol("else") # ternary form

infix_r("or", 30); infix_r("and", 40); prefix("not", 50)

infix("in", 60); infix("not", 60) # not in
infix("is", 60);
infix("<", 60); infix("<=", 60)
infix(">", 60); infix(">=", 60)
infix("<>", 60); infix("!=", 60); infix("==", 60)

infix("|", 70); infix("^", 80); infix("&", 90)

infix("<<", 100); infix(">>", 100)

infix("+", 110); infix("-", 110)

infix("*", 120); infix("/", 120); infix("//", 120)
infix("%", 120)

prefix("-", 130); prefix("+", 130); prefix("~", 130)

infix_r("**", 140)

symbol(".", 150); symbol("[", 150); symbol("(", 150); symbol(")"); symbol("]")

infix_r('+=', 10); infix_r('-=', 10); infix_r('*=', 10); infix_r('/=', 10); infix_r('//=', 10); infix_r('%=', 10); infix_r('>>=', 10); infix_r('<<=', 10); infix_r('**=', 10)

symbol("(name)").nud = lambda self: self
symbol("(literal)").nud = lambda self: self
symbol('(constant)').nud = lambda self: self

#symbol("(end)")
symbol(';')
symbol(',')

def led(self, left):
    if token.category != Token.Category.NAME:
        raise Error('expected an attribute name', token)
    self.append_child(left)
    self.append_child(tokensn)
    next_token()
    return self
symbol('.').led = led

def led(self, left):
    self.function_call = True
    self.append_child(left) # (
    if token.value(source) != ')':
        while True:
            self.append_child(expression())
            if token.value(source) == '=':
                next_token()
                self.append_child(expression())
            else:
                self.children.append(None)
            if token.value(source) != ',':
                break
            advance(',') # (
    advance(')')
    return self
symbol('(').led = led

def nud(self):
    comma = False # ((
    if token.value(source) != ')':
        while True:
            if token.value(source) == ')':
                break
            self.append_child(expression())
            if token.value(source) != ',':
                break
            comma = True
            advance(',')
    advance(')')
    if len(self.children) == 0 or comma:
        self.tuple = True
    return self
symbol('(').nud = nud # )

def led(self, left):
    self.append_child(left)
    if token.value(source) == ':':
        self.slicing = True
        self.children.append(None)
        next_token()
        if token.value(source) == ':':
            self.children.append(None)
            next_token() # [
        if token.value(source) == ']': # for `dirs[:] = ...`
            next_token()
            return self
    self.append_child(expression())
    if token.value(source) == ':':
        self.slicing = True
        next_token() # [[[
        if token.value(source) != ']':
            self.append_child(expression())
            if token.value(source) == ':':
                next_token()
                if token.value(source) != ']':
                    self.append_child(expression())
        else:
            self.children.append(None)
    advance(']')
    return self
symbol('[').led = led

def nud(self):
    self.is_list = True
    if token.value(source) != ']':
        while True: # [[
            if token.value(source) == ']':
                break
            self.append_child(expression())
            if token.value(source) != ',':
                break
            advance(',')
    advance(']')
    return self
symbol('[').nud = nud # ]

def nud(self): # {{{
    if token.value(source) != '}':
        while True:
            if token.value(source) == '}':
                break
            self.append_child(expression())
            advance(':')
            self.append_child(expression())
            if token.value(source) != ',':
                break
            advance(',')
    advance('}')
    return self
symbol('{').nud = nud
symbol('}')

def led(self, left):
    self.append_child(left)
    self.append_child(expression())
    advance('else')
    self.append_child(expression())
    return self
symbol('if').led = led

symbol(':'); symbol('='); symbol('->')

def nud(self):
    global scope
    prev_scope = scope
    scope = Scope([])
    scope.parent = prev_scope
    if token.value(source) != ':':
        while True:
            if token.category != Token.Category.NAME:
                raise Error('expected an argument name', token)
            tokensn.scope = scope
            scope.add_var(token.value(source))
            self.append_child(tokensn)
            next_token()
            if token.value(source) == '=':
                next_token()
                self.append_child(expression())
            else:
                self.children.append(None)
            if token.value(source) != ',':
                break
            advance(',')
    advance(':')
    self.append_child(expression())
    scope = prev_scope
    return self
symbol('lambda').nud = nud

def led(self, left):
    global scope
    prev_scope = scope
    scope = for_scope = Scope([])
    scope.parent = prev_scope
    def set_scope_recursive(sn):
        if sn.scope == prev_scope:
            sn.scope = scope
        elif sn.scope.parent == prev_scope: # for nested list comprehensions
            sn.scope.parent = scope
        else: # this `sn.scope` was already processed
            assert(sn.scope.parent == scope)
        for child in sn.children:
            if child != None:
                set_scope_recursive(child)
    set_scope_recursive(left)
    tokensn.scope = scope
    scope.add_var(token.value(source))

    self.append_child(left)
    self.append_child(tokensn)
    next_token()
    scope = prev_scope
    advance('in')
    if_lbp = symbol('if').lbp
    symbol('if').lbp = 0
    self.append_child(expression())
    symbol('if').lbp = if_lbp
    if token.value(source) == 'if':
        scope = for_scope
        next_token()
        self.append_child(expression())
        scope = prev_scope

    return self
symbol('for', 20).led = led

# multitoken operators

def led(self, left):
    if token.value(source) != 'in':
        raise Error('invalid syntax', token)
    next_token()
    self.append_child(left)
    self.append_child(expression(60))
    return self
symbol('not').led = led

def led(self, left):
    if token.value(source) == 'not':
        next_token()
        self.is_not = True
    self.append_child(left)
    self.append_child(expression(60))
    return self
symbol('is').led = led

def parse_internal(this_node, one_line_scope = False):
    global token

    def new_scope(node, func_args = None):
        if token.value(source) != ':':
            raise Error('expected `:`', Token(tokens[tokeni-1].end, tokens[tokeni-1].end, tokens[tokeni-1].category))
        next_token()
        if token.category != Token.Category.INDENT: # handling of `if ...: break`
            if one_line_scope:
                raise Error('unexpected `:` (only one `:` in one line is allowed)', tokens[tokeni-1])
            parse_internal(node, True)
            return
        global scope
        prev_scope = scope
        scope = Scope(func_args)
        scope.parent = prev_scope
        next_token()
        parse_internal(node)
        scope = prev_scope
        if token != None:
            tokensn.scope = scope

    def expected(ch):
        if token.value(source) != ch:
            raise Error('expected `'+ch+'`', token)
        next_token()

    def expected_name(what_name):
        next_token()
        if token.category != Token.Category.NAME:
            raise Error('expected ' + what_name, token)
        token_value = token.value(source)
        next_token()
        return token_value

    while token != None:
        if token.category == Token.Category.KEYWORD:
            global scope

            if token.value(source) == 'import':
                node = ASTImport()
                next_token()
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected module name', token)
                    module_name = token.value(source)
                    node.modules.append(module_name)

                    # Process module [transpile it if necessary]
                    if module_name not in ('sys', 'tempfile', 'os', 'time', 'datetime', 'math', 're', 'random', 'collections'):
                        module_file_name = os.path.join(os.path.dirname(file_name), module_name).replace('\\', '/') # `os.path.join()` is needed for case when `os.path.dirname(file_name)` is empty string, `replace('\\', '/')` is needed for passing 'tests/parser/errors.txt'
                        try:
                            modulefstat = os.stat(module_file_name + '.py')
                        except FileNotFoundError:
                            raise Error('can not import module `' + module_name + "`: file '" + module_file_name + ".py' is not found", token)

                        _11l_file_mtime = 0
                        if os.path.isfile(module_file_name + '.11l'):
                            _11l_file_mtime = os.stat(module_file_name + '.11l').st_mtime
                        if _11l_file_mtime == 0 \
                                or modulefstat.st_mtime       > _11l_file_mtime \
                                or os.stat(__file__).st_mtime > _11l_file_mtime \
                                or os.stat(os.path.dirname(__file__) + '/tokenizer.py').st_mtime > _11l_file_mtime:
                            module_source = open(module_file_name + '.py', encoding = 'utf-8-sig').read()
                            open(module_file_name + '.11l', 'w', encoding = 'utf-8', newline = "\n").write(parse_and_to_str(tokenizer.tokenize(module_source), module_source, module_file_name + '.py'))

                    scope.add_var(module_name, True, '(Module)')
                    next_token()
                    if token.value(source) != ',':
                        break
                    next_token()

                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'from':
                next_token()
                advance('typing')
                advance('import')
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected name', token)
                    next_token()
                    if token.value(source) != ',':
                        break
                    next_token()

                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()
                continue

            elif token.value(source) == 'def':
                node = ASTFunctionDefinition()
                node.function_name = expected_name('function name')
                scope.add_var(node.function_name, True)

                if token.value(source) != '(': # )
                    raise Error('expected `(` after function name', token) # )(

                next_token()
                was_default_argument = False
                while token.value(source) != ')':
                    if token.value(source) == '*':
                        assert(node.first_named_only_argument == None)
                        node.first_named_only_argument = len(node.function_arguments)
                        next_token()
                        advance(',')
                        continue
                    if token.category != Token.Category.NAME:
                        raise Error('expected function\'s argument name', token)
                    func_arg_name = token.value(source)
                    next_token()
                    type_ = ''
                    if token.value(source) == ':': # this is a type hint
                        next_token()
                        expr = expression()
                        type_ = expr.to_str() if expr.token.category != Token.Category.STRING_LITERAL else expr.to_str()[1:-1]
                    if token.value(source) == '=':
                        next_token()
                        default = expression()
                        was_default_argument = True
                    else:
                        if was_default_argument and node.first_named_only_argument == None:
                            raise Error('non-default argument follows default argument', tokens[tokeni-1])
                        default = None
                    node.function_arguments.append((func_arg_name, default, type_)) # ((
                    if token.value(source) not in ',)':
                        raise Error('expected `,` or `)` in function\'s arguments list', token)
                    if token.value(source) == ',':
                        next_token()

                next_token()
                if token.value(source) == '->':
                    next_token()
                    if token.value(source) == 'None':
                        node.function_return_type = 'None'
                        next_token()
                    else:
                        node.function_return_type = expression().to_str()

                if source[token.end:token.end+7] == ' # -> &':
                    node.function_return_type += '&'

                new_scope(node, map(lambda arg: arg[0], node.function_arguments))

                if len(node.children) == 0: # needed for:
                    n = ASTPass()           # class FileToStringProxy:
                    n.parent = node         #     def __init__(self):
                    node.children.append(n) #         self.result = []

                # Detect virtual functions and assign `virtual_category`
                if type(this_node) == ASTClassDefinition:
                    if this_node.base_class_node != None:
                        for child in this_node.base_class_node.children:
                            if type(child) == ASTFunctionDefinition and child.function_name == node.function_name:
                                if child.virtual_category == ASTFunctionDefinition.VirtualCategory.NO:
                                    if child.function_return_type == '':
                                        raise Error('please specify return type of virtual function', tokens[child.tokeni])
                                    if len(child.children) and type(child.children[0]) == ASTException and child.children[0].expression.symbol.id == '(' and child.children[0].expression.children[0].token.value(source) == 'NotImplementedError': # )
                                        child.virtual_category = ASTFunctionDefinition.VirtualCategory.ABSTRACT
                                    else:
                                        child.virtual_category = ASTFunctionDefinition.VirtualCategory.NEW
                                node.virtual_category = ASTFunctionDefinition.VirtualCategory.OVERRIDE
                                if node.function_return_type == '': # specifying return type of overriden virtual functions is not necessary — it can be taken from original virtual function definition
                                    node.function_return_type = child.function_return_type
                                break

            elif token.value(source) == 'class':
                node = ASTClassDefinition()
                node.class_name = expected_name('class name')
                scope.add_var(node.class_name, True, '(Class)', node = node)

                if token.value(source) == '(':
                    node.base_class_name = expected_name('base class name')
                    if node.base_class_name != 'Exception':
                        base_class = scope.find(node.base_class_name)
                        if base_class == None:
                            raise Error('class `' + node.base_class_name + '` is not defined', tokens[tokeni-1])
                        if base_class.type != '(Class)':
                            raise Error('expected a class name', tokens[tokeni-1])
                        assert(type(base_class.node) == ASTClassDefinition)
                        node.base_class_node = base_class.node
                    expected(')')

                new_scope(node)

            elif token.value(source) == 'pass':
                node = ASTPass()
                next_token()

            elif token.value(source) == 'if':
                if peek_token().value(source) == '__name__':
                    node = ASTStart()
                    next_token()
                    next_token()
                    assert(token.value(source) == '==')
                    next_token()
                    assert(token.value(source) in ("'__main__'", '"__main__"'))
                    next_token()
                    new_scope(node)
                else:
                    node = ASTIf()
                    next_token()
                    node.set_expression(expression())
                    new_scope(node)

                    n = node
                    while token != None and token.value(source) in ('elif', 'else'):
                        if token.value(source) == 'elif':
                            n.else_or_elif = ASTElseIf()
                            n.else_or_elif.parent = n
                            n = n.else_or_elif
                            next_token()
                            n.set_expression(expression())
                            new_scope(n)
                        if token != None and token.value(source) == 'else':
                            n.else_or_elif = ASTElse()
                            n.else_or_elif.parent = n
                            next_token()
                            new_scope(n.else_or_elif)
                            break

            elif token.value(source) == 'while':
                node = ASTWhile()
                next_token()
                node.set_expression(expression())
                new_scope(node)

            elif token.value(source) == 'for':
                node = ASTFor()
                next_token()
                prev_scope = scope
                scope = Scope(None)
                scope.parent = prev_scope

                node.loop_variables = [token.value(source)]
                scope.add_var(node.loop_variables[0], True)
                next_token()
                while token.value(source) == ',':
                    next_token()
                    node.loop_variables.append(token.value(source))
                    scope.add_var(token.value(source), True)
                    next_token()
                advance('in')
                node.set_expression(expression())
                new_scope(node)
                scope = prev_scope

                if token != None and token.value(source) == 'else':
                    node.was_no_break = ASTNodeWithChildren()
                    node.was_no_break.parent = node
                    next_token()
                    new_scope(node.was_no_break)

            elif token.value(source) == 'continue':
                node = ASTContinue()
                next_token()
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'break':
                node = ASTBreak()
                next_token()
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'return':
                node = ASTReturn()
                next_token()
                if token.category in (Token.Category.DEDENT, Token.Category.STATEMENT_SEPARATOR):
                    node.expression = None
                else:
                    node.set_expression(expression())
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) in ('nonlocal', 'global'):
                nonlocal_or_global = token.value(source)
                next_token()
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected ' + nonlocal_or_global + ' variable name', token)
                    if nonlocal_or_global == 'nonlocal':
                        scope.nonlocals.add(token.value(source))
                    else:
                        scope.globals.add(token.value(source))
                    next_token()
                    if token.value(source) == ',':
                        next_token()
                    else:
                        break
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()
                continue

            elif token.value(source) == 'assert':
                node = ASTAssert()
                next_token()
                node.set_expression(expression())
                if token.value(source) == ',':
                    next_token()
                    node.set_expression2(expression())
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'raise':
                node = ASTException()
                next_token()
                node.set_expression(expression())
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'try':
                node = ASTExceptionTry()
                next_token()
                new_scope(node)

            elif token.value(source) == 'except':
                node = ASTExceptionCatch()
                prev_scope = scope
                scope = Scope(None)
                scope.parent = prev_scope
                node.exception_object_type = expected_name('exception object type name')
                if token.value(source) == '.':
                    next_token()
                    node.exception_object_type += ':' + token.value(source)
                    next_token()
                if token.value(source) != ':':
                    advance('as')
                    if token.category != Token.Category.NAME:
                        raise Error('expected exception object name', token)
                    node.exception_object_name = token.value(source)
                    scope.add_var(node.exception_object_name, True)
                    next_token()
                new_scope(node)
                scope = prev_scope

            else:
                raise Error('unrecognized statement started with keyword', token)

        elif token.category == Token.Category.NAME and peek_token().value(source) == '=':
            name_token = token
            node = ASTExprAssignment()
            node.set_dest_expression(tokensn)
            next_token()
            next_token()
            node.set_expression(expression())
            node.add_var = scope.add_var(name_token.value(source), False, 'str' if node.expression.token.category == Token.Category.STRING_LITERAL or (node.expression.function_call and node.expression.children[0].token_str() == 'str') else None, name_token)
            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

        elif token.category == Token.Category.NAME and peek_token().value(source) == ':': # this is type hint
            name_token = token
            var = token.value(source)
            next_token()
            advance(':')
            if token.category not in (Token.Category.NAME, Token.Category.STRING_LITERAL):
                raise Error('expected type name', token)
            type_ = token.value(source) if token.category == Token.Category.NAME else token.value(source)[1:-1]
            type_token = token
            next_token()
            scope.add_var(var, True, type_, name_token)
            type_args = []
            if token.value(source) == '[':
                next_token()
                while token.value(source) != ']':
                    if token.value(source) == '[': # for `Callable[[str, int], str]`
                        next_token()
                        type_arg = token.value(source)
                        next_token()
                        while token.value(source) == ',':
                            next_token()
                            type_arg += ',' + token.value(source)
                            next_token()
                        advance(']')
                        type_args.append(type_arg)
                    elif peek_token().value(source) == '[': # ] # for `table : List[List[List[str]]] = []` and `empty_list : List[List[str]] = []`
                        type_arg = token.value(source)
                        next_token()
                        nesting_level = 0
                        while True:
                            type_arg += token.value(source)
                            if token.value(source) == '[':
                                next_token()
                                nesting_level += 1
                            elif token.value(source) == ']':
                                next_token()
                                nesting_level -= 1
                                if nesting_level == 0:
                                    break
                            else:
                                assert(token.category == Token.Category.NAME)
                                next_token()
                        type_args.append(type_arg)
                    else:
                        type_args.append(token.value(source))
                        next_token() # [[
                    if token.value(source) not in ',]':
                        raise Error('expected `,` or `]` in type\'s arguments list', token)
                    if token.value(source) == ',':
                        next_token()
                next_token()

            if token != None and token.value(source) == '=':
                node = ASTAssignmentWithTypeHint()
                next_token()
                node.set_expression(expression())
            else:
                node = ASTTypeHint()
                if source[type_token.end:type_token.end+4] == ' # &':
                    node.is_reference = True
                if not (token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)):
                    raise Error('expected end of statement', token)
            node.type_token = type_token
            node.var = var
            node.type = type_
            node.type_args = type_args

            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

        elif token.category == Token.Category.DEDENT:
            next_token()
            if token.category == Token.Category.STATEMENT_SEPARATOR: # Token.Category.EOF
                next_token()
                assert(token == None)
            return

        else:
            node_expression = expression()
            if token != None and token.value(source) == '=':
                node = ASTExprAssignment()
                if node_expression.token.category == Token.Category.NAME:
                    node.add_var = scope.add_var(node_expression.token.value(source))
                node.set_dest_expression(node_expression)
                next_token()
                node.set_expression(expression())
            else:
                node = ASTExpression()
                node.set_expression(node_expression)
                if not (token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)):
                    raise Error('expected end of statement', token)
            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

            if (type(node) == ASTExprAssignment and node_expression.token_str() == '.' and node_expression.children[0].token_str() == 'self' and node.expression.token_str() == '[' and len(node.expression.children) == 0 # ] # skip `self.* = []` because `create_array({})` is meaningless
                    and type(this_node) == ASTFunctionDefinition and this_node.function_name == '__init__'): # only in constructors
                continue

        def check_vars_defined(sn : SymbolNode):
            if sn.token.category == Token.Category.NAME:
                if not (sn.parent and sn.parent.token.value(source) == '.') or sn is sn.parent.children[0]: # in `a.b` only `a` [first child] is checked
                    sn.scope_prefix = sn.scope.find_and_get_prefix(sn.token.value(source), sn.token)
            else:
                if sn.function_call:
                    check_vars_defined(sn.children[0])
                    for i in range(1, len(sn.children), 2):
                        if sn.children[i+1] == None:
                            check_vars_defined(sn.children[i])
                        else:
                            check_vars_defined(sn.children[i+1]) # checking of named arguments (sn.children[i]) is skipped
                else:
                    for child in sn.children:
                        if child != None:
                            check_vars_defined(child)
        node.walk_expressions(check_vars_defined)

        node.parent = this_node
        this_node.children.append(node)

        if one_line_scope:
            return

    return

tokens    = []
source    = ''
tokeni    = -1
token     = Token(0, 0, Token.Category.STATEMENT_SEPARATOR)
scope     = Scope(None)
tokensn   = SymbolNode(token)
file_name = ''

def parse_and_to_str(tokens_, source_, file_name_):
    if len(tokens_) == 0: return ASTProgram()
    global tokens, source, tokeni, token, scope, tokensn, file_name
    prev_tokens    = tokens
    prev_source    = source
    prev_tokeni    = tokeni
    prev_token     = token
    prev_scope     = scope
    prev_tokensn   = tokensn
    prev_file_name = file_name
    tokens = tokens_ + [Token(len(source_), len(source_), Token.Category.STATEMENT_SEPARATOR)]
    source = source_
    tokeni = -1
    token = None
    scope = Scope(None)
    for pytype in python_types_to_11l:
        scope.add_var(pytype)
    file_name = file_name_
    next_token()
    p = ASTProgram()
    parse_internal(p)

    def check_for_and_or(node):
        def f(e : SymbolNode):
            if e.symbol.id == 'or' and \
              (e.children[0].symbol.id == 'and' or e.children[1].symbol.id == 'and'):
                if e.children[0].symbol.id == 'and':
                    start = e.children[0].children[0].leftmost()
                    end = e.children[1].rightmost()
                    midend = e.children[0].children[1].rightmost()
                    midstart = e.children[0].children[1].leftmost()
                else:
                    start = e.children[0].leftmost()
                    end = e.children[1].children[1].rightmost()
                    midend = e.children[1].children[0].rightmost()
                    midstart = e.children[1].children[0].leftmost()
                raise Error("relative precedence of operators `and` and `or` is undetermined; please add parentheses this way:\n`("
                    + source[start:midend  ] + ')' + source[midend  :end] + "`\nor this way:\n`"
                    + source[start:midstart] + '(' + source[midstart:end] + ')`', Token(start, end, Token.Category.OPERATOR_OR_DELIMITER))
            for child in e.children:
                if child != None:
                    f(child)

        node.walk_expressions(f)
        node.walk_children(check_for_and_or)
    check_for_and_or(p)

    def transformations(node):
        if isinstance(node, ASTNodeWithChildren):
            index = 0
            while index < len(node.children):
                child = node.children[index]
                if index < len(node.children) - 1 and type(child) == ASTExprAssignment and child.dest_expression.token.category == Token.Category.NAME and type(node.children[index+1]) == ASTIf and node.children[index+1].else_or_elif: # transform if-elif-else chain into switch
                    if_node = node.children[index+1]
                    var_name = child.dest_expression.token.value(source)
                    was_break = False
                    while True:
                        if type(if_node) == ASTElse:
                            break
                        if not (if_node.expression.symbol.id == '==' and if_node.expression.children[0].token.category == Token.Category.NAME and if_node.expression.children[0].token.value(source) == var_name):
                            was_break = True
                            break
                        if_node = if_node.else_or_elif
                        if if_node == None:
                            break
                    if not was_break:
                        switch_node = ASTSwitch()
                        switch_node.set_expression(child.expression)
                        if_node = node.children[index+1]
                        while True:
                            case = ASTSwitch.Case()
                            case.parent = switch_node
                            case.set_expression(SymbolNode(Token(0, 0, Token.Category.KEYWORD), 'E') if type(if_node) == ASTElse else if_node.expression.children[1])
                            case.children = if_node.children
                            for child in case.children:
                                child.parent = case
                            switch_node.cases.append(case)

                            if type(if_node) == ASTElse:
                                break
                            if_node = if_node.else_or_elif
                            if if_node == None:
                                break
                        node.children.pop(index)
                        node.children.pop(index)
                        node.children.insert(index, switch_node)
                        switch_node.parent = node
                        continue # to update child = node.children[index]

                if index < len(node.children) - 1 and type(child) == ASTExpression and child.expression.symbol.id == '-=' and child.expression.children[1].token.value(source) == '1' \
                        and type(node.children[index+1]) == ASTIf and len(node.children[index+1].expression.children) == 2 \
                        and node.children[index+1].expression.children[0].token.value(source) == child.expression.children[0].token.value(source): # transform `nesting_level -= 1 \n if nesting_level == 0:` into `if --nesting_level == 0`
                    child.expression.parent = node.children[index+1].expression#.children[0].parent
                    node.children[index+1].expression.children[0] = child.expression
                    node.children.pop(index)
                    continue

                if type(child) == ASTFor: # detect `for ... in os.walk(...)` and remove `dirs[:] = ...` statement
                    if child.expression.symbol.id == '(' and child.expression.children[0].symbol.id == '.' \
                            and child.expression.children[0].children[0].token_str() == 'os' \
                            and child.expression.children[0].children[1].token_str() == 'walk': # )
                        child.os_walk = True
                        assert(len(child.loop_variables) == 3)
                        c0 = child.children[0]
                        if (type(c0) == ASTExprAssignment and c0.dest_expression.symbol.id == '[' # ]
                                                          and len(c0.dest_expression.children) == 2
                                                          and c0.dest_expression.children[1] == None
                                                          and c0.dest_expression.children[0].token_str() == child.loop_variables[1]
                                                          and c0.expression.symbol.id == '[' # ]
                                                          and len(c0.expression.children) == 1
                                                          and c0.expression.children[0].symbol.id == 'for'
                                                          and len(c0.expression.children[0].children) == 4
                                                          and c0.expression.children[0].children[1].to_str()
                                                           == c0.expression.children[0].children[0].to_str()):
                            child.dir_filter = c0.expression.children[0].children[1].to_str() + ' -> ' + c0.expression.children[0].children[3].to_str()
                            child.children.pop(0)

                    elif child.expression.symbol.id == '(' and child.expression.children[0].token_str() == 'enumerate': # )
                        assert(len(child.loop_variables) == 2)
                        set_index_node = ASTExprAssignment()
                        set_index_node.set_dest_expression(SymbolNode(Token(0, 0, Token.Category.NAME), child.loop_variables[0]))
                        child.loop_variables.pop(0)
                        set_index_node.set_expression(SymbolNode(Token(0, 0, Token.Category.NAME), 'L.index' + (' + ' + child.expression.children[3].to_str() if len(child.expression.children) >= 5 else '')))
                        set_index_node.add_var = True
                        set_index_node.parent = child
                        child.children.insert(0, set_index_node)
                        child.expression.children[0].parent = child.expression.parent
                        child.expression.children[0].ast_parent = child.expression.ast_parent
                        child.expression = child.expression.children[1]

                elif type(child) == ASTFunctionDefinition: # detect function's arguments changing/modification inside this function, and add qualifier `=` to changing ones
                    for fargi in range(len(child.function_arguments)):
                        farg = child.function_arguments[fargi][0]
                        found = False
                        def detect_argument_modification(node):
                            if type(node) == ASTExprAssignment and node.dest_expression.token_str() == farg:
                                nonlocal found
                                found = True
                                return
                            def f(e : SymbolNode):
                                if e.symbol.id[-1] == '=' and e.symbol.id not in ('==', '!=') and e.children[0].token_str() == farg: # +=, -=, *=, /=, etc.
                                    nonlocal found
                                    found = True
                            node.walk_expressions(f)
                            node.walk_children(detect_argument_modification)
                        detect_argument_modification(child)
                        if found:
                            child.function_arguments[fargi] = ('=' + child.function_arguments[fargi][0], child.function_arguments[fargi][1], child.function_arguments[fargi][2])

                index += 1

        node.walk_children(transformations)
    transformations(p)

    s = p.to_str() # call `to_str()` moved here [from outside] because it accesses global variables `source` (via `token.value(source)`) and `tokens` (via `tokens[ti]`)

    tokens    = prev_tokens
    source    = prev_source
    tokeni    = prev_tokeni
    token     = prev_token
    scope     = prev_scope
    tokensn   = prev_tokensn
    file_name = prev_file_name

    return s
