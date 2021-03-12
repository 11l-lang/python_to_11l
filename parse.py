try:
    from python_to_11l.tokenizer import Token
    import python_to_11l.tokenizer as tokenizer
except ImportError:
    from tokenizer import Token
    import tokenizer
from typing import List, Tuple, Dict, Callable
from enum import IntEnum
import os, re, eldf

class Scope:
    parent : 'Scope'
    class Var:
        type : str
        node : 'ASTNode'

        def __init__(self, type, node):
            assert(type is not None)
            self.type = type
            self.node = node

        def serialize_to_dict(self):
            node = None
            if type(self.node) == ASTFunctionDefinition:
                node = self.node.serialize_to_dict()
            return {'type': self.type, 'node': node}

        def deserialize_from_dict(self, d):
            if d['node'] is not None:
                self.node = ASTFunctionDefinition()
                self.node.deserialize_from_dict(d['node'])

    vars : Dict[str, Var]
    nonlocals_copy : set
    nonlocals : set
    globals   : set
    is_function : bool
    is_lambda_or_for = False

    def __init__(self, func_args):
        self.parent = None
        if func_args is not None:
            self.is_function = True
            self.vars = dict(map(lambda x: (x[0], Scope.Var(x[1], None)), func_args))
        else:
            self.is_function = False
            self.vars = {}
        self.nonlocals_copy = set()
        self.nonlocals = set()
        self.globals   = set()

    def serialize_to_dict(self, imported_modules):
        ids_dict = {'Imported modules': imported_modules}
        for name, id in self.vars.items():
            if name not in python_types_to_11l and not id.type.startswith('('): # )
                ids_dict[name] = id.serialize_to_dict()
        return ids_dict

    def deserialize_from_dict(self, d):
        for name, id_dict in d.items():
            if name != 'Imported modules':
                id = Scope.Var(id_dict['type'], None)
                id.deserialize_from_dict(id_dict)
                self.vars[name] = id

    def add_var(self, name, error_if_already_defined = False, type = '', err_token = None, node = None):
        s = self
        while True:
            if name in s.nonlocals_copy or name in s.nonlocals or name in s.globals:
                return False
            if s.is_function:
                break
            s = s.parent
            if s is None:
                break

        if not (name in self.vars):
            s = self
            while True:
                if name in s.vars:
                    return False
                if s.is_function:
                    break
                s = s.parent
                if s is None:
                    break
            self.vars[name] = Scope.Var(type, node)
            return True
        elif error_if_already_defined:
            raise Error('redefinition of already defined variable is not allowed', err_token if err_token is not None else token)
        return False

    def find_and_get_prefix(self, name, token):
        if name == 'self':
            return ''
        if name in ('isinstance', 'len', 'super', 'print', 'input', 'ord', 'chr', 'range', 'zip', 'all', 'any', 'abs', 'pow', 'sum', 'product', 'open', 'min', 'max', 'divmod', 'hex', 'bin', 'map', 'list', 'tuple', 'dict', 'set', 'sorted', 'reversed', 'filter', 'reduce', 'round', 'enumerate', 'hash', 'copy', 'deepcopy', 'NotImplementedError', 'ValueError', 'IndexError'):
            return ''

        s = self
        while True:
            if name in s.nonlocals_copy:
                return '@='
            if name in s.nonlocals:
                return '@'
            if name in s.globals:
                return ':'
            if s.is_function and not s.is_lambda_or_for:
                break
            s = s.parent
            if s is None:
                break

        capture_level = 0
        s = self
        while True:
            if name in s.vars:
                if s.parent is None: # variable is declared in the global scope
                    if s.vars[name].type == '(Module)':
                        return ':::'
                    return ':' if capture_level > 0 else ''
                else:
                    return capture_level*'@'
            if s.is_function:
                capture_level += 1
            s = s.parent
            if s is None:
                if name in ('id',):
                    return ''
                raise Error('undefined identifier', token)

    def find(self, name):
        s = self
        while True:
            id = s.vars.get(name)
            if id is not None:
                return id
            s = s.parent
            if s is None:
                return None

    def var_type(self, name):
        id = self.find(name)
        return id.type if id is not None else None

scope : Scope

class Module:
    scope : Scope

    def __init__(self, scope):
        self.scope = scope

modules : Dict[str, Module] = {}

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
    function_call = False
    iterable_unpacking = False
    tuple   = False
    is_list = False
    is_set  = False
    def is_dict(self): return self.symbol.id == '{' and not self.is_set # }
    slicing = False
    is_not  = False
    skip_find_and_get_prefix = False
    scope_prefix : str = ''
    scope : Scope
    token_str_override : str

    def __init__(self, token, token_str_override = None):
        self.token = token
        self.children = []
        self.scope = scope
        self.token_str_override = token_str_override

    def var_type(self):
        if self.is_parentheses():
            return self.children[0].var_type()
        if self.symbol.id == '*' and self.children[0].var_type() == 'List':
            return 'List'
        if self.symbol.id == '+' and (self.children[0].var_type() == 'List' or self.children[1].var_type() == 'List'):
            return 'List'
        if self.is_list:
            return 'List'
        #if self.symbol.id == '[' and not self.is_list and self.children[0].var_type() == 'str': # ]
        if self.symbol.id == '[' and self.children[0].var_type() == 'str': # ]
            return 'str'
        if self.symbol.id == '*' and self.children[1].var_type() == 'str':
            return 'str'
        if self.token.category == Token.Category.STRING_LITERAL:
            return 'str'
        if self.symbol.id == '.':
            if self.children[0].token_str() == 'os' and self.children[1].token_str() == 'pathsep':
                return 'str'
            return None
        if self.symbol.id == 'if':
            t0 = self.children[0].var_type()
            if t0 is not None:
                return t0
            return self.children[2].var_type()
        if self.function_call and self.children[0].token_str() == 'str':
            return 'str'
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
            if len(self.children) == 0:
                return self.token.end + 1
            return (self.children[-1] or self.children[-2]).rightmost() + 1

        return self.children[-1].rightmost()

    def left_to_right_token(self):
        return Token(self.leftmost(), self.rightmost(), Token.Category.NAME)

    def token_str(self):
        return self.token.value(source) if not self.token_str_override else self.token_str_override

    def is_parentheses(self):
        return self.symbol.id == '(' and not self.tuple and not self.function_call # )

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
            if self.scope_prefix == ':' and ((self.parent and self.parent.function_call and self is self.parent.children[0]) or (self.token_str()[0].isupper() and self.token_str() != self.token_str().upper()) or self.token_str() in python_types_to_11l): # global functions and types do not require prefix `:` because global functions and types are ok, but global variables are not so good and they should be marked with `:`
                return self.token_str()
            if self.token_str() == 'self' and (self.parent is None or (self.parent.symbol.id != '.' and self.parent.symbol.id != 'lambda')):
                parent = self
                while parent.parent is not None:
                    parent = parent.parent
                ast_parent = parent.ast_parent
                while ast_parent is not None:
                    if isinstance(ast_parent, ASTFunctionDefinition):
                        if len(ast_parent.function_arguments) and ast_parent.function_arguments[0][0] == 'self' and isinstance(ast_parent.parent, ASTClassDefinition):
                            return '(.)'
                        break
                    ast_parent = ast_parent.parent
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
                    if len(n) <= 2: # ultrashort hexadecimal number
                        n = '0'*(2-len(n)) + n
                        return n[:1] + "'" + n[1:]
                    elif len(n) <= 4: # short hexadecimal number
                        n = '0'*(4-len(n)) + n
                        return n[:2] + "'" + n[2:]
                    else:
                        number_with_separators = ''
                        j = len(n)
                        while j > 4:
                            number_with_separators = "'" + n[j-4:j] + number_with_separators
                            j -= 4
                        return sign + '0'*(4-j) + n[0:j] + number_with_separators
            if n[-1] in 'jJ':
                n = n[:-1] + 'i'
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
                    if s == R'"\\"' or s == R"'\\'":
                        return R'‘\’'
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
            return not((child1 is None or child1.token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL))
                   and (child2 is None or child2.token.category in (Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL)))

        if self.symbol.id == '(': # )
            if self.function_call:
                if self.children[0].symbol.id == '.':
                    c01 = self.children[0].children[1].token_str()
                    if self.children[0].children[0].symbol.id == '{' and c01 == 'get': # } # replace `{'and':'&', 'or':'|', 'in':'C'}.get(self.symbol.id, 'symbol-' + self.symbol.id)` with `(S .symbol.id {‘and’ {‘&’}; ‘or’ {‘|’}; ‘in’ {‘C’} E ‘symbol-’(.symbol.id)})`
                        parenthesis = ('(', ')') if self.parent is not None else ('', '')
                        return parenthesis[0] + self.children[0].to_str() + parenthesis[1]
                    if c01 == 'join' and not (self.children[0].children[0].symbol.id == '.' and self.children[0].children[0].children[0].token_str() == 'os'): # replace `', '.join(arr)` with `arr.join(‘, ’)`
                        assert(len(self.children) == 3)
                        return (self.children[1].to_str() if self.children[1].token.category == Token.Category.NAME or self.children[1].symbol.id == 'for' or self.children[1].function_call else '(' + self.children[1].to_str() + ')') + '.join(' + (self.children[0].children[0].children[0].to_str() if self.children[0].children[0].is_parentheses() else self.children[0].children[0].to_str()) + ')'
                    if c01 == 'split' and len(self.children) == 5 and not (self.children[0].children[0].token_str() == 're'): # split() second argument [limit] in 11l is similar to JavaScript, Ruby and PHP, but not Python
                        return self.children[0].to_str() + '(' + self.children[1].to_str() + ', ' + self.children[3].to_str() + ' + 1)'
                    if c01 == 'split' and len(self.children) == 1:
                        return self.children[0].to_str() + '_py()' # + '((‘ ’, "\\t", "\\r", "\\n"), group_delimiters\' 1B)'
                    if c01 == 'is_integer' and len(self.children) == 1: # `x.is_integer()` -> `fract(x) == 0`
                        return 'fract(' + self.children[0].children[0].to_str() + ') == 0'
                    if c01 == 'bit_length' and len(self.children) == 1: # `x.bit_length()` -> `bit_length(x)`
                        return 'bit_length(' + self.children[0].children[0].to_str() + ')'
                    repl = {'startswith':'starts_with', 'endswith':'ends_with', 'find':'findi', 'rfind':'rfindi',
                            'lower':'lowercase', 'islower':'is_lowercase', 'upper':'uppercase', 'isupper':'is_uppercase', 'isdigit':'is_digit', 'isalpha':'is_alpha',
                            'timestamp':'unix_time', 'lstrip':'ltrim', 'rstrip':'rtrim', 'strip':'trim',
                            'appendleft':'append_left', 'extendleft':'extend_left', 'popleft':'pop_left', 'issubset':'is_subset'}.get(c01, '')
                    if repl != '': # replace `startswith` with `starts_with`, `endswith` with `ends_with`, etc.
                        c00 = self.children[0].children[0].to_str()
                        if repl == 'uppercase' and c00.endswith('[2..]') and self.children[0].children[0].children[0].symbol.id == '(' and self.children[0].children[0].children[0].children[0].token_str() == 'hex': # ) # `hex(x)[2:].upper()` -> `hex(x)`
                            return 'hex(' + self.children[0].children[0].children[0].children[1].to_str() + ')'
                        #assert(len(self.children) == 3)
                        res = c00 + '.' + repl + '('
                        def is_char(child):
                            ts = child.token_str()
                            return child.token.category == Token.Category.STRING_LITERAL and (len(ts) == 3 or (ts[:2] == '"\\' and len(ts) == 4))
                        if repl.endswith('trim') and len(self.children) == 1: # `strip()` -> `trim((‘ ’, "\t", "\r", "\n"))`
                            res += '(‘ ’, "\\t", "\\r", "\\n")'
                        elif repl.endswith('trim') and not is_char(self.children[1]): # `"...".strip("\t ")` -> `"...".trim(Array[Char]("\t "))`
                            assert(len(self.children) == 3)
                            res += 'Array[Char](' + self.children[1].to_str() + ')'
                        else:
                            for i in range(1, len(self.children), 2):
                                assert(self.children[i+1] is None)
                                res += self.children[i].to_str()
                                if i < len(self.children)-2:
                                    res += ', '
                        return res + ')'
                    if self.children[0].children[0].symbol.id == '(' and \
                       self.children[0].children[0].children[0].token_str() == 'open' and \
                   len(self.children[0].children[0].children) == 5 and \
                       self.children[0].children[0].children[4] is None and \
                       self.children[0].children[0].children[3].token_str() in ("'rb'", '"rb"') and \
                       self.children[0].children[1].token_str() == 'read': # ) # transform `open(fname, 'rb').read()` into `File(fname).read_bytes()`
                        assert(self.children[0].children[0].children[2] is None)
                        return 'File(' + self.children[0].children[0].children[1].to_str() + ').read_bytes()'
                    if c01 == 'total_seconds': # `delta.total_seconds()` -> `delta.seconds`
                        assert(len(self.children) == 1)
                        return self.children[0].children[0].to_str() + '.seconds'
                    if c01 == 'conjugate' and len(self.children) == 1: # `c.conjugate()` -> `conjugate(c)`
                        return 'conjugate(' + self.children[0].children[0].to_str() + ')'
                    if c01 == 'readlines': # `f.readlines()` -> `f.read_lines(1B)`
                        assert(len(self.children) == 1)
                        return self.children[0].children[0].to_str() + ".read_lines(1B)"
                    if c01 == 'readline': # `f.readline()` -> `f.read_line(1B)`
                        assert(len(self.children) == 1)
                        return self.children[0].children[0].to_str() + ".read_line(1B)"
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
                        if source[self.children[1].token.end + 2 : self.children[1].token.end + 3] != '#':
                            raise Error('to use `defaultdict` the type of dict keys must be specified in the comment', self.children[0].children[1].token)
                        sl = slice(self.children[1].token.end + 3, source.find("\n", self.children[1].token.end + 3))
                        return 'DefaultDict[' + trans_type(source[sl].lstrip(' '), self.scope, Token(sl.start, sl.stop, Token.Category.NAME)) + ', ' \
                                              + trans_type(self.children[1].token_str(), self.scope, self.children[1].token) + ']()'
                    if self.children[0].children[0].token_str() == 'collections' and self.children[0].children[1].token_str() == 'deque': # `collections.deque() # ValueType` -> `Deque[ValueType]()`
                        if len(self.children) == 3:
                            return 'Deque(' + self.children[1].to_str() + ')'
                        assert(len(self.children) == 1)
                        if source[self.token.end + 2 : self.token.end + 3] != '#':
                            raise Error('to use `deque` the type of deque values must be specified in the comment', self.children[0].children[1].token)
                        sl = slice(self.token.end + 3, source.find("\n", self.token.end + 3))
                        return 'Deque[' + trans_type(source[sl].lstrip(' '), self.scope, Token(sl.start, sl.stop, Token.Category.NAME)) + ']()'
                    if self.children[0].children[0].token_str() == 'int' and self.children[0].children[1].token_str() == 'from_bytes':
                        assert(len(self.children) == 5)
                        if not (self.children[3].token.category == Token.Category.STRING_LITERAL and self.children[3].token_str()[1:-1] == 'little'):
                            raise Error("only 'little' byteorder supported so far", self.children[3].token)
                        return "Int(bytes' " + self.children[1].to_str() + ')'
                    if self.children[0].children[0].token_str() == 'random' and self.children[0].children[1].token_str() == 'shuffle':
                        return 'random:shuffle(&' + self.children[1].to_str() + ')'
                    if self.children[0].children[0].token_str() == 'random' and self.children[0].children[1].token_str() == 'randint':
                        return 'random:(' + self.children[1].to_str() + ' .. ' + self.children[3].to_str() + ')'
                    if self.children[0].children[0].token_str() == 'random' and self.children[0].children[1].token_str() == 'randrange':
                        return 'random:(' + self.children[1].to_str() + (' .< ' + self.children[3].to_str() if len(self.children) == 5 else '') + ')'
                    if self.children[0].children[0].token_str() == 'heapq':
                        res = 'minheap:' + {'heappush':'push', 'heappop':'pop', 'heapify':'heapify'}[self.children[0].children[1].token_str()] + '(&'
                        for i in range(1, len(self.children), 2):
                            assert(self.children[i+1] is None)
                            res += self.children[i].to_str()
                            if i < len(self.children)-2:
                                res += ', '
                        return res + ')'
                    if self.children[0].children[0].token_str() == 'itertools' and self.children[0].children[1].token_str() == 'count': # `itertools.count(1)` -> `1..`
                        return self.children[1].to_str() + '..'

                func_name = self.children[0].to_str()
                if func_name == 'str':
                    func_name = 'String'
                elif func_name in ('int', 'Int64'):
                    if func_name == 'int':
                        func_name = 'Int'
                    if len(self.children) == 5:
                        return func_name + '(' + self.children[1].to_str() + ", radix' " + self.children[3].to_str() + ')'
                elif func_name == 'float':
                    if len(self.children) == 3 and self.children[1].token.category == Token.Category.STRING_LITERAL and self.children[1].token_str()[1:-1].lower() in ('infinity', 'inf'):
                        return 'Float.infinity'
                    func_name = 'Float'
                elif func_name == 'complex':
                    func_name = 'Complex'
                elif func_name == 'list': # `list(map(...))` -> `map(...)`
                    if len(self.children) == 3 and self.children[1].symbol.id == '(' and self.children[1].children[0].token_str() == 'range': # ) # `list(range(...))` -> `Array(...)`
                        parens = True#len(self.children[1].children) == 7 # if true, then this is a range with step
                        return 'Array' + '('*parens + self.children[1].to_str() + ')'*parens
                    assert(len(self.children) == 3)
                    if self.children[1].symbol.id == '(' and self.children[1].children[0].token_str() in ('map', 'product', 'zip'): # )
                        return self.children[1].to_str()
                    else:
                        return 'Array(' + self.children[1].to_str() + ')'
                elif func_name == 'tuple': # `tuple(sorted(...))` -> `tuple_sorted(...)`
                    assert(len(self.children) == 3)
                    if self.children[1].function_call and self.children[1].children[0].token_str() == 'sorted':
                        return 'tuple_' + self.children[1].to_str()
                elif func_name == 'dict':
                    func_name = 'Dict'
                elif func_name == 'set': # `set() # KeyType` -> `Set[KeyType]()`
                    if len(self.children) == 3:
                        return 'Set(' + self.children[1].to_str() + ')'
                    assert(len(self.children) == 1)
                    if source[self.token.end + 2 : self.token.end + 3] != '#':
                        # if self.parent is None and type(self.ast_parent) == ASTExprAssignment \
                        #         and self.ast_parent.dest_expression.symbol.id == '.' \
                        #         and self.ast_parent.dest_expression.children[0].token_str() == 'self' \
                        #         and type(self.ast_parent.parent) == ASTFunctionDefinition \
                        #         and self.ast_parent.parent.function_name == '__init__':
                        #     return 'Set()'
                        raise Error('to use `set` the type of set keys must be specified in the comment', self.children[0].token)
                    sl = slice(self.token.end + 3, source.find("\n", self.token.end + 3))
                    return 'Set[' + trans_type(source[sl].lstrip(' '), self.scope, Token(sl.start, sl.stop, Token.Category.NAME)) + ']()'
                elif func_name == 'open':
                    func_name = 'File'
                    mode = '‘r’'
                    for i in range(1, len(self.children), 2):
                        if self.children[i+1] is None:
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
                elif func_name == 'product':
                    func_name = 'cart_product'
                elif func_name == 'deepcopy':
                    func_name = 'copy'
                elif func_name == 'print' and self.iterable_unpacking:
                    func_name = 'print_elements'

                if func_name == 'len': # replace `len(container)` with `container.len`
                    assert(len(self.children) == 3)
                    if isinstance(self.ast_parent, (ASTIf, ASTWhile)) if self.parent is None else self.parent.symbol.id == 'if': # `if len(arr)` -> `I !arr.empty`
                        return '!' + self.children[1].to_str() + '.empty'
                    if len(self.children[1].children) == 2 and self.children[1].symbol.id not in ('.', '['): # ]
                        return '(' + self.children[1].to_str() + ')' + '.len'
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
                    b = len(self.children[3].children) > 1 and self.children[3].symbol.id not in ('(', '[') # ])
                    c1 = self.children[1].to_str()
                    return '('*b + self.children[3].to_str() + ')'*b + '.' + func_name + '(' + {'int':'Int', 'float':'Float', 'str':'String'}.get(c1, c1) + ')'
                elif func_name == 'reduce':
                    if len(self.children) == 5: # replace `reduce(function, iterable)` with `iterable.reduce(function)`
                        return self.children[3].to_str() + '.reduce(' + self.children[1].to_str() + ')'
                    else: # replace `reduce(function, iterable, initial)` with `iterable.reduce(initial, function)`
                        assert(len(self.children) == 7)
                        return self.children[3].to_str() + '.reduce(' + self.children[5].to_str() + ', ' + self.children[1].to_str() + ')'
                elif func_name == 'super': # replace `super()` with `T.base`
                    assert(len(self.children) == 1)
                    return 'T.base'
                elif func_name == 'range':
                    assert(3 <= len(self.children) <= 7)
                    parenthesis = ('(', ')') if self.parent is not None and (self.parent.symbol.id == 'for' or (self.parent.function_call and self.parent.children[0].token_str() in ('map', 'filter', 'reduce'))) else ('', '')
                    if len(self.children) == 3: # replace `range(e)` with `(0 .< e)`
                        space = ' ' * range_need_space(self.children[1], None)
                        c1 = self.children[1].to_str()
                        if c1.endswith(' + 1'): # `range(e + 1)` -> `0 .. e`
                            return parenthesis[0] + '0' + space + '..' + space + c1[:-4] + parenthesis[1]
                        return parenthesis[0] + '0' + space + '.<' + space + c1 + parenthesis[1]
                    else:
                        rangestr = ' .< ' if range_need_space(self.children[1], self.children[3]) else '.<'
                        if len(self.children) == 5: # replace `range(b, e)` with `(b .< e)`
                            if self.children[3].token.category == Token.Category.NUMERIC_LITERAL and self.children[3].token_str().replace('_', '').isdigit() and \
                               self.children[1].token.category == Token.Category.NUMERIC_LITERAL and self.children[1].token_str().replace('_', '').isdigit(): # if `b` and `e` are numeric literals, then ...
                                return parenthesis[0] + self.children[1].token_str().replace('_', '') + '..' + str(int(self.children[3].token_str().replace('_', '')) - 1) + parenthesis[1] # ... replace `range(b, e)` with `(b..e-1)`
                            c3 = self.children[3].to_str()
                            if c3.endswith(' + 1'): # `range(a, b + 1)` -> `a .. b`
                                return parenthesis[0] + self.children[1].to_str() + rangestr.replace('<', '.') + c3[:-4] + parenthesis[1]
                            return parenthesis[0] + self.children[1].to_str() + rangestr + c3 + parenthesis[1]
                        else: # replace `range(b, e, step)` with `(b .< e).step(step)`
                            return '(' + self.children[1].to_str() + rangestr + self.children[3].to_str() + ').step(' + self.children[5].to_str() + ')'
                elif func_name == 'print':
                    first_named_argument = len(self.children)
                    for i in range(1, len(self.children), 2):
                        if self.children[i+1] is not None:
                            first_named_argument = i
                            break

                    sep = '‘ ’'
                    for i in range(first_named_argument, len(self.children), 2):
                        assert(self.children[i+1] is not None)
                        if self.children[i].to_str() == 'sep':
                            sep = self.children[i+1].to_str()
                            break

                    def surround_with_sep(s, before, after):
                        if (sep in ('‘ ’', '‘’') # special case for ‘ ’ and ‘’
                             or sep[0] == s[0]): # ‘`‘sep’‘str’‘sep’` -> `‘sepstrsep’`’|‘`"sep""str""sep"` -> `"sepstrsep"`’
                            return s[0] + sep[1:-1]*before + s[1:-1] + sep[1:-1]*after + s[-1]
                        else: # `"sep"‘str’"sep"`|`‘sep’"str"‘sep’`
                            return sep*before + s + sep*after

                    def parenthesize_if_needed(child):
                        #if child.token.category in (Token.Category.NAME, Token.Category.NUMERIC_LITERAL) or child.symbol.id == '[': # ] # `print(‘Result: ’3)` is currently not supported in 11l
                        if child.token.category == Token.Category.NAME or child.symbol.id in ('[', '('): # )]
                            return child.to_str()
                        else:
                            return '(' + child.to_str() + ')'

                    res = 'print('
                    for i in range(1, first_named_argument, 2):
                        if i == 1: # it's the first agrument
                            if i == first_named_argument - 2: # it's the only argument — ‘no sep is required’/‘no parentheses are required’
                                res += self.children[i].to_str()
                            elif self.children[i].token.category == Token.Category.STRING_LITERAL:
                                res += surround_with_sep(self.children[i].to_str(), False, True)
                            else:
                                res += parenthesize_if_needed(self.children[i])
                        else:
                            if self.children[i].token.category == Token.Category.STRING_LITERAL:
                                if self.children[i-2].token.category == Token.Category.STRING_LITERAL:
                                    raise Error('consecutive string literals in `print()` are not supported', self.children[i].token)
                                res += surround_with_sep(self.children[i].to_str(), True, i != first_named_argument - 2)
                            else:
                                if self.children[i-2].token.category != Token.Category.STRING_LITERAL:
                                    res += sep
                                res += parenthesize_if_needed(self.children[i])

                    for i in range(first_named_argument, len(self.children), 2):
                        if self.children[i].to_str() != 'sep':
                            if len(res) > len('print('): # )
                                res += ', '
                            res += self.children[i].to_str() + "' " + self.children[i+1].to_str()

                    return res + ')'
                else:
                    if ':' in func_name:
                        colon_pos = func_name.rfind(':')
                        module_name = func_name[:colon_pos].replace(':', '.')
                        if module_name in modules:
                            tid = modules[module_name].scope.find(func_name[colon_pos+1:])
                        else:
                            tid = None
                    elif func_name.startswith('.'):
                        s = self.scope
                        while True:
                            if s.is_function and not s.is_lambda_or_for:
                                tid = s.parent.vars.get(func_name[1:])
                                break
                            s = s.parent
                            if s is None:
                                tid = None
                                break
                    else:
                        tid = self.scope.find(func_name)
                    f_node = tid.node if tid is not None and type(tid.node) == ASTFunctionDefinition else None
                    res = func_name + '('
                    for i in range(1, len(self.children), 2):
                        if self.children[i+1] is None:
                            if f_node is not None:
                                fargs = f_node.function_arguments[i//2 + int(func_name.startswith('.'))]
                                arg_type_name = fargs[2]
                                if arg_type_name.startswith(('List[', 'Dict[', 'DefaultDict[')) or (arg_type_name != '' and trans_type(arg_type_name, self.scope, self.children[i].token).endswith('&')) or fargs[3] == '&': # ]]]
                                    res += '&'
                            res += self.children[i].to_str()
                        else:
                            ci_str = self.children[i].to_str()
                            res += ci_str + "' "
                            if f_node is not None:
                                for farg in f_node.function_arguments:
                                    if farg[0] == ci_str:
                                        if farg[2].startswith(('List[', 'Dict[')): # ]]
                                            res += '&'
                                        break
                            res += self.children[i+1].to_str()
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
                parenthesis = ('(', ')') if self.parent is not None else ('', '')
                res = parenthesis[0] + 'S ' + self.children[1].to_str() + ' {'
                for i in range(0, len(self.children[0].children), 2):
                    res += self.children[0].children[i].to_str() + ' {' + self.children[0].children[i+1].to_str() + '}'
                    if i < len(self.children[0].children)-2:
                        res += '; '
                return res + '}' + parenthesis[1]
            else:
                c0 = self.children[0].to_str()
                if self.slicing:
                    if len(self.children) == 2: # `a = b[:]` -> `a = copy(b)`
                        assert(self.children[1] is None)
                        return 'copy(' + c0 + ')'
                    if c0.startswith('bin(') and len(self.children) == 3 and self.children[1].token_str() == '2' and self.children[2] is None: # ) # `bin(x)[2:]` -> `bin(x)`
                        return c0
                    if len(self.children) == 4 and self.children[1] is None and self.children[2] is None and self.children[3].symbol.id == '-' and len(self.children[3].children) == 1 and self.children[3].children[0].token_str() == '1': # replace `result[::-1]` with `reversed(result)`
                        return 'reversed(' + c0 + ')'
                    def for_negative_bound(c):
                        child = self.children[c]
                        if child is None:
                            return None
                        r = child.to_str()
                        if r[0] == '-': # hacky implementation of ‘this rule’[https://docs.python.org/3/reference/simple_stmts.html]:‘If either bound is negative, the sequence's length is added to it.’
                            r = '(len)' + r
                        return r
                    space = ' ' * range_need_space(self.children[1], self.children[2])
                    fnb2 = for_negative_bound(2)
                    s = (for_negative_bound(1) or '0') + space + '.' + ('<' + space + fnb2 if fnb2 else '.')
                    if len(self.children) == 4 and self.children[3] is not None:
                        s = '(' + s + ').step(' + self.children[3].to_str() + ')'
                    return c0 + '[' + s + ']'
                elif self.children[1].to_str() == '-1':
                    return c0 + '.last'
                else:
                    c1 = self.children[1].to_str()
                    return (c0 + '['
                        + '(len)'*(c1[0] == '-') # hacky implementation of ‘this rule’[https://docs.python.org/3/reference/simple_stmts.html]:‘the subscript must yield an integer. If it is negative, the sequence's length is added to it.’
                        + c1 + ']')

        elif self.symbol.id == '{': # }
            if len(self.children) == 0:
                return 'Dict()'

            if self.is_set:
                is_not_for = self.children[0].symbol.id != 'for'
                res = 'Set(' + '['*is_not_for
                for i in range(len(self.children)):
                    res += self.children[i].to_str()
                    if i < len(self.children)-1:
                        res += ', '
                return res + ']'*is_not_for + ')'

            if self.children[-1].symbol.id == 'for':
                assert(len(self.children) == 2)
                c = self.children[1]
                c2s = c.children[2].to_str()
                return 'Dict(' + (c2s[1:-1] if c.children[2].function_call and c.children[2].children[0].token_str() == 'range' else c2s) + ', ' + c.children[1].to_str() + ' -> (' + self.children[0].to_str() + ', ' + c.children[0].to_str() + '))'

            res = '['
            for i in range(0, len(self.children), 2):
                res += self.children[i].to_str() + ' = ' + self.children[i+1].to_str()
                if i < len(self.children)-2:
                    res += ', '
            return res + ']'

        elif self.symbol.id == 'lambda':
            r = '(' if len(self.children) != 3 else ''
            for i in range(0, len(self.children)-1, 2):
                r += self.children[i].token_str()
                if self.children[i+1] is not None:
                    r += ' = ' + self.children[i+1].to_str()
                if i < len(self.children)-3:
                    r += ', '
            if len(self.children) != 3: r += ')'
            return r + ' -> ' + self.children[-1].to_str()

        elif self.symbol.id == 'for':
            if self.children[2].token_str() == 'for': # this is a multiloop
                if self.children[2].children[2].token_str() == 'for': # this is a multiloop3
                    filtered = len(self.children[2].children[2].children) == 4
                    res = 'multiloop' + '_filtered'*filtered + '(' + self.children[2].children[0].to_str() + ', ' + self.children[2].children[2].children[0].to_str() + ', ' + self.children[2].children[2].children[2].to_str()
                    fparams = ', (' + self.children[1].token_str() + ', ' + self.children[2].children[1].token_str() + ', ' + self.children[2].children[2].children[1].token_str() + ') -> '
                    if filtered:
                        res += fparams + self.children[2].children[2].children[3].to_str()
                    res += fparams + self.children[0].to_str() + ')'
                    return res

                filtered = len(self.children[2].children) == 4
                res = 'multiloop' + '_filtered'*filtered + '(' + self.children[2].children[0].to_str() + ', ' + self.children[2].children[2].to_str()
                fparams = ', (' + self.children[1].token_str() + ', ' + self.children[2].children[1].token_str() + ') -> '
                if filtered:
                    res += fparams + self.children[2].children[3].to_str()
                res += fparams + self.children[0].to_str() + ')'
                return res

            res = self.children[2].children[0].children[0].to_str() if self.children[2].symbol.id == '(' and len(self.children[2].children) == 1 and self.children[2].children[0].symbol.id == '.' and len(self.children[2].children[0].children) == 2 and self.children[2].children[0].children[1].token_str() == 'items' else self.children[2].to_str() # )
            if len(self.children) == 4:
                res += '.filter(' + self.children[1].to_str() + ' -> ' + self.children[3].to_str() + ')'
            if self.children[1].to_str() != self.children[0].to_str():
                res +=    '.map(' + self.children[1].to_str() + ' -> ' + self.children[0].to_str() + ')'
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
            if self.children[1].token_str() == 'None':
                return self.children[0].to_str() + (' != ' if self.is_not else ' == ') + 'N'
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
                    if self.children[0].token_str() in ('math', 'cmath'):
                        c1 = self.children[1].to_str()
                        if c1 not in ('e', 'pi'):
                            if c1 == 'fabs': c1 = 'abs'
                            return c1
                    r = self.children[0].token_str() + ':' + self.children[1].to_str()
                    return {'tempfile:gettempdir': 'fs:get_temp_dir', 'os:path': 'fs:path', 'os:pathsep': 'os:env_path_sep', 'os:sep': 'fs:path:sep', 'os:system': 'os:', 'os:listdir': 'fs:list_dir', 'os:walk': 'fs:walk_dir',
                    'os:mkdir': 'fs:create_dir', 'os:makedirs': 'fs:create_dirs', 'os:remove': 'fs:remove_file', 'os:rmdir': 'fs:remove_dir', 'os:rename': 'fs:rename',
                    'time:time': 'Time().unix_time', 'time:sleep': 'sleep', 'datetime:datetime': 'Time', 'datetime:date': 'Time', 'datetime:timedelta': 'TimeDelta', 're:compile': 're:',
                    'random:random': 'random:'}.get(r, r)

                if self.children[0].symbol.id == '.' and self.children[0].children[0].scope_prefix == ':::':
                    if self.children[0].children[0].token_str() == 'datetime':
                        if self.children[0].children[1].token_str() == 'datetime':
                            if self.children[1].token_str() == 'now': # `datetime.datetime.now()` -> `Time()`
                                return 'Time'
                            if self.children[1].token_str() == 'fromtimestamp': # `datetime.datetime.fromtimestamp()` -> `time:from_unix_time()`
                                return 'time:from_unix_time'
                            if self.children[1].token_str() == 'strptime': # `datetime.datetime.strptime()` -> `time:strptime()`
                                return 'time:strptime'
                        if self.children[0].children[1].token_str() == 'date' and self.children[1].token_str() == 'today': # `datetime.date.today()` -> `time:today()`
                            return 'time:today'
                    if self.children[0].children[0].token_str() == 'os' and self.children[0].children[1].token_str() == 'path':
                        r = {'pathsep':'os:env_path_sep', 'isdir':'fs:is_dir', 'isfile':'fs:is_file', 'islink':'fs:is_symlink',
                             'dirname':'fs:path:dir_name', 'basename':'fs:path:base_name', 'abspath':'fs:path:absolute', 'relpath':'fs:path:relative',
                             'getsize':'fs:file_size', 'splitext':'fs:path:split_ext'}.get(self.children[1].token_str(), '')
                        if r != '':
                            return r

                if len(self.children[0].children) == 2 and self.children[0].children[0].scope_prefix == ':::' and self.children[0].children[0].token_str() != 'sys': # for `os.path.join()` [and also take into account `sys.argv.index()`]
                    return self.children[0].to_str() + ':' + self.children[1].to_str()

                if self.children[0].to_str() == 'self':
                    parent = self
                    while parent.parent:
                        if parent.parent.symbol.id == 'for' and id(parent.parent.children[0]) == id(parent):
                            return '@.' + self.children[1].to_str()
                        parent = parent.parent
                        if parent.symbol.id == 'lambda':
                            if len(parent.children) >= 3 and parent.children[0].token_str() == 'self':
                                return 'self.' + self.children[1].to_str()
                            return '@.' + self.children[1].to_str()
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
                c1 = self.children[1].to_str()
                return self.children[0].to_str() + ' [+]= ' + (c1[1:-1] if len(self.children[1].children) == 1 and c1.startswith('[') else c1) # ]
            elif self.symbol.id == '+=' and self.children[1].token.value(source) == '1':
                return self.children[0].to_str() + '++'
            elif self.symbol.id == '-=' and self.children[1].token.value(source) == '1':
                return '--' + self.children[0].to_str() if self.parent else self.children[0].to_str() + '--'
            elif self.symbol.id == '+=' and ((self.children[0].token.category == Token.Category.NAME and self.children[0].var_type() == 'str')
                                          or (self.children[1].symbol.id == '+' and len(self.children[1].children) == 2 and
                                             (self.children[1].children[0].token.category == Token.Category.STRING_LITERAL
                                           or self.children[1].children[1].token.category == Token.Category.STRING_LITERAL))
                                           or self.children[1].token.category == Token.Category.STRING_LITERAL):
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
            elif self.symbol.id == '+' and (self.children[0].var_type() == 'List' or self.children[1].var_type() == 'List'):
                return self.children[0].to_str() + ' [+] ' + self.children[1].to_str()
            elif self.symbol.id == '<=' and self.children[0].symbol.id == '<=': # replace `'0' <= ch <= '9'` with `ch C ‘0’..‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' .. ' if range_need_space(self.children[0].children[0], self.children[1]) else '..') + self.children[1].to_str()
            elif self.symbol.id == '<'  and self.children[0].symbol.id == '<=': # replace `'0' <= ch <  '9'` with `ch C ‘0’.<‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' .< ' if range_need_space(self.children[0].children[0], self.children[1]) else '.<') + self.children[1].to_str()
            elif self.symbol.id == '<=' and self.children[0].symbol.id == '<' : # replace `'0' <  ch <= '9'` with `ch C ‘0’<.‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' <. ' if range_need_space(self.children[0].children[0], self.children[1]) else '<.') + self.children[1].to_str()
            elif self.symbol.id == '<'  and self.children[0].symbol.id == '<' : # replace `'0' <= ch <= '9'` with `ch C ‘0’<.<‘9’`
                return self.children[0].children[1].to_str() + ' C ' + self.children[0].children[0].to_str() + (' <.< ' if range_need_space(self.children[0].children[0], self.children[1]) else '<.<') + self.children[1].to_str()
            elif self.symbol.id == '==' and self.children[0].symbol.id == '(' and self.children[0].children[0].to_str() == 'len' and self.children[1].token.value(source) == '0': # ) # replace `len(arr) == 0` with `arr.empty`
                return self.children[0].children[1].to_str() + '.empty'
            elif self.symbol.id == '!=' and self.children[0].symbol.id == '(' and self.children[0].children[0].to_str() == 'len' and self.children[1].token.value(source) == '0': # ) # replace `len(arr) != 0` with `!arr.empty`
                return '!' + self.children[0].children[1].to_str() + '.empty'
            elif self.symbol.id in ('==', '!=') and self.children[1].symbol.id == '.' and len(self.children[1].children) == 2 and self.children[1].children[1].token_str().isupper(): # replace `token.category == Token.Category.NAME` with `token.category == NAME`
                #self.skip_find_and_get_prefix = True # this is not needed here because in AST there is still `Token.Category.NAME`, not just `NAME`
                return self.children[0].to_str() + ' ' + self.symbol.id + ' ' + self.children[1].children[1].token_str()
            elif self.symbol.id in ('==', '!=') and self.children[0].function_call and self.children[0].children[0].token_str() == 'id' and self.children[1].function_call and self.children[1].children[0].token_str() == 'id': # replace `id(a) == id(b)` with `&a == &b`
                return '&' + self.children[0].children[1].token_str() + ' ' + self.symbol.id + ' &' + self.children[1].children[1].token_str()
            elif self.symbol.id == '%' and self.children[0].token.category == Token.Category.STRING_LITERAL:
                add_parentheses = self.children[1].symbol.id != '(' or self.children[1].function_call # )
                fmtstr = self.children[0].to_str()
                nfmtstr = ''
                i = 0
                while i < len(fmtstr):
                    if fmtstr[i] == '#':
                        nfmtstr += '##'
                        i += 1
                        continue

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
                            period_pos = 0
                            i += 1
                            if fmtstr[i] == '-': # left align
                                nfmtstr += '<'
                                i += 1
                            if fmtstr[i:i+1] == '0' and fmtstr[i+1:i+2].isdigit(): # zero padding
                                nfmtstr += '0'
                            while i < len(fmtstr) and fmtstr[i].isdigit():
                                before_period = before_period*10 + ord(fmtstr[i]) - ord('0')
                                i += 1
                            if fmtstr[i:i+1] == '.':
                                period_pos = i
                                i += 1
                                after_period = 0
                                while i < len(fmtstr) and fmtstr[i].isdigit():
                                    after_period = after_period*10 + ord(fmtstr[i]) - ord('0')
                                    i += 1
                            if fmtstr[i:i+1] in ('d', 'i'):
                                if before_period != 0:
                                    nfmtstr += str(before_period)
                                else:
                                    nfmtstr += '.'#'.0' # `#.0` corresponds to `%.0f` rather than `%i` or `%d`, and `'%i' % (1.7)` = `1`, but `‘#.0’.format(1.7)` = `2`
                            elif fmtstr[i:i+1] == 's':
                                if before_period != 0:
                                    nfmtstr += str(before_period)
                                else:
                                    nfmtstr += '.'
                            elif fmtstr[i:i+1] == 'f':
                                if before_period != 0:
                                    b = before_period
                                    if after_period != 0:
                                        b -= after_period + 1
                                    if b > 1:
                                        nfmtstr += str(b)
                                nfmtstr += '.' + str(after_period)
                            elif fmtstr[i:i+1] == 'g':
                                nfmtstr += str(before_period)
                                if period_pos != 0:
                                    raise Error('precision in %g conversion type is not supported', Token(self.children[0].token.start + period_pos, self.children[0].token.start + i, Token.Category.STRING_LITERAL))
                            else:
                                tpos = self.children[0].token.start + i
                                raise Error('unsupported format character `' + fmtstr[i:i+1] + '`', Token(tpos, tpos, Token.Category.STRING_LITERAL))
                            i += 1
                        continue

                    nfmtstr += fmtstr[i]
                    i += 1
                return nfmtstr + '.format' + '('*add_parentheses + self.children[1].to_str() + ')'*add_parentheses
            else:
                return self.children[0].to_str() + ' ' + {'and':'&', 'or':'|', 'in':'C', '//':'I/', '//=':'I/=', '**':'^', '**=':'^=', '^':'(+)', '^=':'(+)=', '|':'[|]', '|=':'[|]=', '&':'[&]', '&=':'[&]='}.get(self.symbol.id, self.symbol.id) + ' ' + self.children[1].to_str()
        elif len(self.children) == 3:
            assert(self.symbol.id == 'if')
            c0 = self.children[0].to_str()
            if self.children[1].symbol.id == 'is' and self.children[1].is_not and self.children[1].children[1].token.value(source) == 'None' and self.children[1].children[0].to_str() == c0: # replace `a if a is not None else b` with `a ? b`
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
    imported_modules : List[str] = None

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
    add_vars : List[bool]
    drop_list = False
    is_tuple_assign_expression = False
    dest_expression : SymbolNode
    additional_dest_expressions : List[SymbolNode]

    def __init__(self):
    #     self.add_vars = [] # this is not necessary
        self.additional_dest_expressions = []

    def set_dest_expression(self, dest_expression):
        self.dest_expression = dest_expression
        self.dest_expression.ast_parent = self

    def to_str(self, indent):
        if type(self.parent) == ASTClassDefinition:
            assert(len(self.add_vars) == 1 and self.add_vars[0] and not self.is_tuple_assign_expression)
            return ' ' * (indent*3) + self.dest_expression.to_str() + ' = ' + self.expression.to_str() + "\n"

        if self.dest_expression.slicing:
            s = self.dest_expression.to_str() # [
            if s.endswith(']') and self.expression.function_call and self.expression.children[0].token_str() == 'reversed' and self.expression.children[1].to_str() == s:
                l = len(self.dest_expression.children[0].to_str())
                return ' ' * (indent*3) + s[:l] + '.reverse_range(' + s[l+1:-1]  + ")\n"
            raise Error('slice assignment is not supported', self.dest_expression.left_to_right_token())

        if self.drop_list:
            return ' ' * (indent*3) + self.dest_expression.to_str() + ".drop()\n"

        if self.dest_expression.tuple and len(self.dest_expression.children) == 2 and \
           self.     expression.tuple and len(self.     expression.children) == 2 and \
           self.dest_expression.children[0].to_str() == self.expression.children[1].to_str() and \
           self.dest_expression.children[1].to_str() == self.expression.children[0].to_str():
            return ' ' * (indent*3) + 'swap(&' + self.dest_expression.children[0].to_str() + ', &' + self.dest_expression.children[1].to_str() + ")\n"

        if self.is_tuple_assign_expression or not any(self.add_vars):
            r = ' ' * (indent*3) + self.dest_expression.to_str()
            for ade in self.additional_dest_expressions:
                r += ' = ' + ade.to_str()
            return r + ' = ' + self.expression.to_str() + "\n"
        if all(self.add_vars):
            if self.expression.function_call and self.expression.children[0].token_str() == 'ref':
                assert(len(self.expression.children) == 3)
                return ' ' * (indent*3) + 'V& ' + self.dest_expression.to_str() + ' = ' + self.expression.children[1].to_str() + "\n"
            return ' ' * (indent*3) + 'V ' + self.dest_expression.to_str() + ' = ' + self.expression.to_str() + "\n"

        assert(self.dest_expression.tuple and len(self.dest_expression.children) == len(self.add_vars))
        r = ' ' * (indent*3) + '('
        for i in range(len(self.add_vars)):
            if self.add_vars[i]:
                r += 'V '
            assert(self.dest_expression.children[i].token.category == Token.Category.NAME)
            r += self.dest_expression.children[i].token_str()
            if i < len(self.add_vars)-1:
                r += ', '
        return r + ') = ' + self.expression.to_str() + "\n"

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
            else self.expression.to_str()) + (', ' + self.expression2.to_str() if self.expression2 is not None else '') + ")\n"

    def walk_expressions(self, f):
        if self.expression2 is not None: f(self.expression2)
        super().walk_expressions(f)

python_types_to_11l = {'&':'&', 'int':'Int', 'float':'Float', 'complex':'Complex', 'str':'String', 'Char':'Char', 'Int64':'Int64', 'UInt32':'UInt32', 'Byte':'Byte', 'bool':'Bool', 'None':'N', 'List':'', 'Tuple':'Tuple', 'Dict':'Dict', 'DefaultDict':'DefaultDict', 'Set':'Set', 'IO[str]': 'File',
                       'datetime.date':'Time', 'datetime.datetime':'Time'}

def trans_type(ty, scope, type_token):
    if ty[0] in '\'"':
        assert(ty[-1] == ty[0])
        ty = ty[1:-1]
    t = python_types_to_11l.get(ty)
    if t is not None:
        return t
    else:
        p = ty.find('[')
        if p != -1:
            assert(ty[-1] == ']')
            i = p + 1
            s = i
            nesting_level = 0
            types = []
            while True:
                if ty[i] == '[':
                    nesting_level += 1
                elif ty[i] == ']':
                    if nesting_level == 0:
                        assert(i == len(ty)-1)
                        types.append(trans_type(ty[s:i], scope, type_token))
                        break
                    nesting_level -= 1
                elif ty[i] == ',':
                    if nesting_level == 0: # ignore inner commas
                        if ty[s:i] == '[]' and ty.startswith('Callable['): # ] # for `Callable[[], str]`
                            types.append('()')
                        else:
                            types.append(trans_type(ty[s:i], scope, type_token))
                        i += 1
                        while ty[i] == ' ':
                            i += 1
                        s = i
                        #continue # this is not necessary here
                i += 1
            if ty.startswith('Tuple['): # ]
                return '(' + ', '.join(types) + ')'
            if ty.startswith('Dict['): # ]
                assert(len(types) == 2)
                return '[' + types[0] + ' = ' + types[1] + ']'
            if ty.startswith('Callable['): # ]
                assert(len(types) == 2)
                return '(' + types[0] + ' -> ' + types[1] + ')'
            if p == 0: # for `Callable`
                assert(len(types) != 0)
                parens = len(types) > 1
                return '('*parens + ', '.join(types) + ')'*parens
            return trans_type(ty[:p], scope, type_token) + '[' + ', '.join(types) + ']'

        assert(ty.find(',') == -1)

        if '.' in ty: # for `category : Token.Category`
            return ty # [-TODO: generalize-]

        id = scope.find(ty)
        if id is None:
            raise Error('class `' + ty + '` is not defined', type_token)
        if id.type != '(Class)':
            raise Error('`' + ty + '`: expected a class name (got variable' + (' of type `' + id.type + '`' if id.type != '' else '') + ')', type_token)
        return ty + '&'*id.node.is_inout

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
            if self.type_args[0] == '':
                args = '()'
            else:
                tt = self.type_args[0].split(',')
                args = ', '.join(self.trans_type(ty) for ty in tt)
                if len(tt) > 1:
                    args = '(' + args + ')'
            return ' ' * (indent*3) + '(' + args + ' -> ' + self.trans_type(self.type_args[1]) + ') ' + self.var
        elif self.type == 'Optional':
            assert(len(self.type_args) == 1)
            return ' ' * (indent*3) + self.trans_type(self.type_args[0]) + ('& ' if self.is_reference else '? ') + self.var
        return ' ' * (indent*3) + self.trans_type(self.type + ('[' + ', '.join(self.type_args) + ']' if len(self.type_args) else '')) + '?'*nullable + '&'*self.is_reference + ' ' + self.var

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
    is_const = False
    function_arguments : List[Tuple[str, str, str, str]]# = [] # (arg_name, default_value, type_name, qualifier)
    first_named_only_argument = None
    class VirtualCategory(IntEnum):
        NO = 0
        NEW = 1
        OVERRIDE = 2
        ABSTRACT = 3
        ASSIGN = 4
    virtual_category = VirtualCategory.NO
    scope : Scope

    def __init__(self):
        super().__init__()
        self.function_arguments = []
        self.scope = scope

    def serialize_to_dict(self):
        return {'function_arguments': ['; '.join(arg) for arg in self.function_arguments]}

    def deserialize_from_dict(self, d):
        self.function_arguments = [arg.split('; ') for arg in d['function_arguments']]

    def to_str(self, indent):
        if self.function_name in ('move', 'copy', 'ref') and type(self.parent) == ASTProgram:
            assert(len(self.function_arguments) == 1)
            return ''

        fargs = []
        for arg in self.function_arguments:
            farg = ''
            default_value = arg[1]
            if arg[2] != '':
                ty = trans_type(arg[2], self.scope, tokens[self.tokeni])
                # if ty.endswith('&'): # fix error ‘expected function's argument name’ at `F trazar(Rayo& =r; prof)` (when there was `r = ...` instead of `rr = ...`)
                #     arg = (arg[0].lstrip('='), arg[1], arg[2])
                farg += ty
                if default_value == 'N':
                    farg += '?'
                    assert(arg[3] == '')
                farg += ' '
                if ty.startswith(('Array[', '[', 'Dict[', 'DefaultDict[')) or arg[3] == '&': # ]]]]
                    farg += '&'
            else:
                if arg[3] == '&':
                    farg += '&'
            farg += arg[0] + ('' if default_value == '' else ' = ' + default_value)
            fargs.append((farg, arg[2] != ''))
        if self.first_named_only_argument is not None:
            fargs.insert(self.first_named_only_argument, ("'", fargs[self.first_named_only_argument][1]))
        if len(self.function_arguments) and self.function_arguments[0][0] == 'self' and type(self.parent) == ASTClassDefinition:
            fargs.pop(0)

        fargs_str = ''
        if len(fargs):
            fargs_str = fargs[0][0]
            prev_type = fargs[0][1]
            for farg in fargs[1:]:
                fargs_str += ('; ' if prev_type and not farg[1] else ', ') + farg[0]
                prev_type = farg[1]

        if self.virtual_category == self.VirtualCategory.ABSTRACT:
            return ' ' * (indent*3) + 'F.virtual.abstract ' + self.function_name + '(' + fargs_str + ') -> ' + trans_type(self.function_return_type, self.scope, tokens[self.tokeni]) + "\n"

        return self.children_to_str(indent, ('F', 'F.virtual.new', 'F.virtual.override', '', 'F.virtual.assign')[self.virtual_category] + '.const'*self.is_const + ' ' +
            {'__init__':'', '__call__':'()', '__and__':'[&]', '__lt__':'<', '__eq__':'==', '__add__':'+', '__sub__':'-', '__mul__':'*', '__str__':'String'}.get(self.function_name, self.function_name)
            + '(' + fargs_str + ')'
            + ('' if self.function_return_type == '' else ' -> ' + trans_type(self.function_return_type, self.scope, tokens[self.tokeni])))

class ASTIf(ASTNodeWithChildren, ASTNodeWithExpression):
    else_or_elif : ASTNode = None

    def walk_expressions(self, f):
        super().walk_expressions(f)
        if self.else_or_elif is not None and isinstance(self.else_or_elif, ASTElseIf):
            self.else_or_elif.walk_expressions(f)

    def walk_children(self, f):
        super().walk_children(f)
        if self.else_or_elif is not None:
            self.else_or_elif.walk_children(f)

    def to_str(self, indent):
        return self.children_to_str(indent, 'I ' + self.expression.to_str()) + (self.else_or_elif.to_str(indent) if self.else_or_elif is not None else '')

class ASTElse(ASTNodeWithChildren):
    def to_str(self, indent):
        return self.children_to_str(indent, 'E')

class ASTElseIf(ASTNodeWithChildren, ASTNodeWithExpression):
    else_or_elif : ASTNode = None

    def walk_expressions(self, f):
        super().walk_expressions(f)
        if self.else_or_elif is not None and isinstance(self.else_or_elif, ASTElseIf):
            self.else_or_elif.walk_expressions(f)

    def walk_children(self, f):
        super().walk_children(f)
        if self.else_or_elif is not None:
            self.else_or_elif.walk_children(f)

    def to_str(self, indent):
        return self.children_to_str(indent, 'E I ' + self.expression.to_str()) + (self.else_or_elif.to_str(indent) if self.else_or_elif is not None else '')

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
        if self.was_no_break is not None:
            self.was_no_break.walk_children(f)

    def to_str(self, indent):
        if self.os_walk:
            dir_filter = ''
            if self.dir_filter is not None:
                dir_filter = ", dir_filter' " + self.dir_filter # (
            return self.children_to_str(indent, 'L(_fname) ' + self.expression.to_str()[:-1] + dir_filter + ", files_only' 0B)\n"
                + ' ' * ((indent+1)*3) + 'V ' + self.loop_variables[0] + " = fs:path:dir_name(_fname)\n"
                + ' ' * ((indent+1)*3) + '[String] ' + self.loop_variables[1] + ', ' + self.loop_variables[2] + "\n"
                + ' ' * ((indent+1)*3) + 'I fs:is_dir(_fname) {' + self.loop_variables[1] + ' [+]= fs:path:base_name(_fname)} E ' + self.loop_variables[2] + ' [+]= fs:path:base_name(_fname)')

        if len(self.loop_variables) == 1:
            r = 'L(' + self.loop_variables[0] + ') ' + (self.expression.children[1].to_str()
                   if self.expression.function_call and self.expression.children[0].token_str() == 'range' and # `L(i) 100` instead of `L(i) 0.<100`
                 len(self.expression.children) == 3 and self.expression.children[1].token.category == Token.Category.NUMERIC_LITERAL else self.expression.to_str())
            if self.expression.token.category == Token.Category.NAME:
                sid = self.expression.scope.find(self.expression.token_str())
                if sid.type in ('Dict', 'DefaultDict'):
                    r += '.keys()'
        elif self.expression.symbol.id == '(' and len(self.expression.children) == 1 and self.expression.children[0].symbol.id == '.' and len(self.expression.children[0].children) == 2 and self.expression.children[0].children[1].token_str() == 'items': # )
            r = 'L(' + ', '.join(self.loop_variables) + ') ' + self.expression.children[0].children[0].to_str()
        else:
            r = 'L(' + ', '.join(self.loop_variables) + ') ' + self.expression.to_str()
            # r = 'L(' + ''.join(self.loop_variables) + ') ' + self.expression.to_str()
            # for index, loop_var in enumerate(self.loop_variables):
            #     r += "\n" + ' ' * ((indent+1)*3) + 'V ' + loop_var + ' = ' + ''.join(self.loop_variables) + '[' + str(index) + ']'
        r = self.children_to_str(indent, r)

        if self.was_no_break is not None:
            r += self.was_no_break.children_to_str(indent, 'L.was_no_break')

        return r

class ASTContinue(ASTNode):
    def to_str(self, indent):
        return ' ' * (indent*3) + "L.continue\n"

class ASTBreak(ASTNode):
    def to_str(self, indent):
        return ' ' * (indent*3) + "L.break\n"

class ASTReturn(ASTNodeWithExpression):
    def to_str(self, indent):
        return ' ' * (indent*3) + 'R' + (' ' + self.expression.to_str() if self.expression is not None else '') + "\n"

    def walk_expressions(self, f):
        if self.expression is not None: f(self.expression)

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
        return self.children_to_str(indent, 'X.catch' + (' ' + self.exception_object_type if self.exception_object_type != '' else '')
                                                      + (' ' + self.exception_object_name if self.exception_object_name != '' else ''))

class ASTDel(ASTNodeWithExpression):
    def to_str(self, indent):
        assert(self.expression.slicing and len(self.expression.children) == 3)
        return ' ' * (indent*3) + self.expression.children[0].to_str() + '.del(' + self.expression.children[1].to_str() + ' .< ' + self.expression.children[2].to_str() + ")\n"

class ASTClassDefinition(ASTNodeWithChildren):
    base_class_name : str = None
    base_class_node : 'ASTClassDefinition' = None
    class_name : str
    is_inout = False

    def find_member_including_base_classes(self, name):
        for child in self.children:
            if isinstance(child, ASTTypeHint) and child.var == name:
                return True
        if self.base_class_node is not None:
            return self.base_class_node.find_member_including_base_classes(name)
        return False

    def to_str(self, indent):
        if self.base_class_name == 'IntEnum':
            r = ' ' * (indent*3) + 'T.enum ' + self.class_name + "\n"
            current_index = 0
            for c in self.children:
                assert(type(c) == ASTExprAssignment and c.expression.token.category == Token.Category.NUMERIC_LITERAL)
                r += ' ' * ((indent+1)*3) + c.dest_expression.to_str()
                if current_index != int(c.expression.token_str()):
                    current_index = int(c.expression.token_str())
                    r += ' = ' + c.expression.token_str()
                current_index += 1
                r += "\n"
            return r
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

def next_token(): # why ‘next_token’: >[https://youtu.be/Nlqv6NtBXcA?t=1203]:‘we'll have an advance method which will fetch the next token’
    global token, tokeni, tokensn
    if token is None and tokeni != -1:
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
                    if token.value(source) in ('V', 'C', 'I', 'E', 'F', 'L', 'N', 'R', 'S', 'T', 'X', 'var', 'fn', 'loop', 'null', 'switch', 'type', 'exception', 'sign'):
                        tokensn.token_str_override = '_' + token.value(source).lower() + '_'
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
        if tokensn.symbol is None:
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

infix_r('+=', 10); infix_r('-=', 10); infix_r('*=', 10); infix_r('/=', 10); infix_r('//=', 10); infix_r('%=', 10); infix_r('>>=', 10); infix_r('<<=', 10); infix_r('**=', 10); infix_r('|=', 10); infix_r('^=', 10); infix_r('&=', 10)

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
            if token.value(source) == '*': # >[https://stackoverflow.com/a/19525681/2692494 <- google:‘python iterable unpacking precedence’]:‘The unpacking `*` is not an operator; it's part of the call syntax.’
                if len(self.children) != 1:
                    raise Error('iterable unpacking is supported only in first agrument', token)
                if not (left.token.category == Token.Category.NAME and left.token_str() == 'print'):
                    raise Error('iterable unpacking is supported only for `print()` function', token)
                self.iterable_unpacking = True
                next_token()

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
        next_token() # [
        if token.value(source) != ']': # for `arr[:]`
            if token.value(source) == ':':
                self.children.append(None)
                next_token()
                self.append_child(expression())
            else:
                self.append_child(expression())
                if token.value(source) == ':':
                    next_token()
                    self.append_child(expression())
    else:
        self.append_child(expression())
        if token.value(source) == ':':
            self.slicing = True
            next_token() # [[
            if token.value(source) != ']':
                if token.value(source) == ':':
                    self.children.append(None)
                    next_token()
                    self.append_child(expression())
                else:
                    self.append_child(expression())
                    if token.value(source) == ':':
                        next_token()
                        self.append_child(expression())
            else:
                self.children.append(None)
    advance(']')
    return self
symbol('[').led = led

def nud(self):
    self.is_list = True
    while True: # [
        if token.value(source) == ']':
            break
        self.append_child(expression())
        if token.value(source) != ',':
            break
        advance(',')
    advance(']')
    return self
symbol('[').nud = nud # ]

def nud(self): # {{{{
    if token.value(source) != '}':
        while True:
            if token.value(source) == '}':
                break
            self.append_child(expression())
            if token.value(source) != ':':
                self.is_set = True
                while True:
                    if token.value(source) != ',':
                        break
                    advance(',')
                    if token.value(source) == '}':
                        break
                    self.append_child(expression())
                break
            advance(':')
            self.append_child(expression())

            if self.children[-1].symbol.id == 'for':
                for_scope = self.children[-1].children[0].scope
                def set_scope_recursive(sn):
                    assert(sn.scope == scope)
                    sn.scope = for_scope
                    for child in sn.children:
                        if child is not None:
                            set_scope_recursive(child)
                set_scope_recursive(self.children[0])
                break

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
    scope.is_lambda_or_for = True
    scope.parent = prev_scope
    if token.value(source) != ':':
        while True:
            if token.category != Token.Category.NAME:
                raise Error('expected an argument name', token)
            tokensn.scope = scope
            scope.add_var(tokensn.token_str())
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
    scope.is_lambda_or_for = True
    scope.parent = prev_scope
    def set_scope_recursive(sn):
        if sn.scope == prev_scope:
            sn.scope = scope
        elif sn.scope.parent == prev_scope: # for nested list comprehensions
            sn.scope.parent = scope
        else: # this `sn.scope` was already processed
            assert(sn.scope.parent == scope)
        for child in sn.children:
            if child is not None:
                set_scope_recursive(child)
    set_scope_recursive(left)
    tokensn.scope = scope
    scope.add_var(tokensn.token_str())

    self.append_child(left)
    self.append_child(tokensn)
    next_token()

    if token.value(source) == ',':
        sn = SymbolNode(Token(token.start, token.start, Token.Category.OPERATOR_OR_DELIMITER))
        sn.symbol = symbol_table['('] # )
        sn.tuple = True
        sn.append_child(self.children.pop())
        self.append_child(sn)
        next_token()
        scope.add_var(tokensn.token_str())
        sn.append_child(tokensn)
        next_token()
        if token.value(source) == ',':
            next_token()
            scope.add_var(tokensn.token_str())
            sn.append_child(tokensn)
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

    if self.children[2].token_str() == 'for': # this is a multiloop
        for_scope.add_var(self.children[2].children[1].token_str())
        def set_scope_recursive(sn):
            sn.scope = scope
            for child in sn.children:
                if child is not None:
                    set_scope_recursive(child)
        set_scope_recursive(self.children[2].children[0])
        def set_for_scope_recursive(sn):
            sn.scope = for_scope
            for child in sn.children:
                if child is not None:
                    set_for_scope_recursive(child)
        if self.children[2].children[2].token_str() == 'for': # this is a multiloop3
            for_scope.add_var(self.children[2].children[2].children[1].token_str())
            if len(self.children[2].children[2].children) == 4:
                set_for_scope_recursive(self.children[2].children[2].children[3])
        else:
            if len(self.children[2].children) == 4:
                set_for_scope_recursive(self.children[2].children[3])

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
        global scope
        prev_scope = scope
        scope = Scope(func_args)
        scope.parent = prev_scope
        if token.category != Token.Category.INDENT: # handling of `if ...: break`, `def ...(...): return ...`, etc.
            if one_line_scope:
                raise Error('unexpected `:` (only one `:` in one line is allowed)', tokens[tokeni-1])
            tokensn.scope = scope # for `if ...: new_var = ...` (though code `if ...: new_var = ...` has no real application, this line is needed for correct error message outputting)
            parse_internal(node, True)
        else:
            next_token()
            parse_internal(node)
        scope = prev_scope
        if token is not None:
            tokensn.scope = scope

    def expected(ch):
        if token.value(source) != ch:
            raise Error('expected `'+ch+'`', token)
        next_token()

    def expected_name(what_name):
        next_token()
        if token.category != Token.Category.NAME:
            raise Error('expected ' + what_name, token)
        token_value = tokensn.token_str()
        next_token()
        return token_value

    def check_vars_defined(sn : SymbolNode):
        if sn.token.category == Token.Category.NAME:
            if sn.parent is None or sn.parent.symbol.id != '.' or sn is sn.parent.children[0]: # in `a.b` only `a` [first child] is checked
                if not sn.skip_find_and_get_prefix:
                    sn.scope_prefix = sn.scope.find_and_get_prefix(sn.token_str(), sn.token)
        else:
            if sn.function_call:
                check_vars_defined(sn.children[0])
                for i in range(1, len(sn.children), 2):
                    if sn.children[i+1] is None:
                        check_vars_defined(sn.children[i])
                    else:
                        check_vars_defined(sn.children[i+1]) # checking of named arguments (sn.children[i]) is skipped
            else:
                for child in sn.children:
                    if child is not None:
                        check_vars_defined(child)

    while token is not None:
        if token.category == Token.Category.KEYWORD:
            global scope

            if token.value(source) == 'import':
                if type(this_node) != ASTProgram:
                    raise Error('only global import statements are supported', token)
                node = ASTImport()
                next_token()
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected module name', token)
                    module_name = token.value(source)
                    while peek_token().value(source) == '.':
                        next_token()
                        next_token()
                        if token.category != Token.Category.NAME:
                            raise Error('expected module name', token)
                        module_name += '.' + token.value(source)
                    node.modules.append(module_name)

                    # Process module [transpile it if necessary]
                    if module_name not in ('sys', 'tempfile', 'os', 'time', 'datetime', 'math', 'cmath', 're', 'random', 'collections', 'heapq', 'itertools', 'eldf'):
                        if this_node.imported_modules is not None:
                            this_node.imported_modules.append(module_name)

                        module_file_name = os.path.join(os.path.dirname(file_name), module_name.replace('.', '/')).replace('\\', '/') # `os.path.join()` is needed for case when `os.path.dirname(file_name)` is empty string, `replace('\\', '/')` is needed for passing 'tests/parser/errors.txt'
                        try:
                            modulefstat = os.stat(module_file_name + '.py')
                        except FileNotFoundError:
                            raise Error('can not import module `' + module_name + "`: file '" + module_file_name + ".py' is not found", token)

                        _11l_file_mtime = 0
                        if os.path.isfile(module_file_name + '.11l'):
                            _11l_file_mtime = os.stat(module_file_name + '.11l').st_mtime
                        modified = _11l_file_mtime == 0 \
                                or modulefstat.st_mtime       > _11l_file_mtime \
                                or os.stat(__file__).st_mtime > _11l_file_mtime \
                                or os.stat(os.path.dirname(__file__) + '/tokenizer.py').st_mtime > _11l_file_mtime \
                                or not os.path.isfile(module_file_name + '.py_global_scope')
                        if not modified: # check for dependent modules modifications
                            py_global_scope = eldf.parse(open(module_file_name + '.py_global_scope', encoding = 'utf-8-sig').read())
                            py_imported_modules = py_global_scope['Imported modules']
                            for m in py_imported_modules:
                                if os.stat(os.path.join(os.path.dirname(module_file_name), m.replace('.', '/') + '.py')).st_mtime > _11l_file_mtime:
                                    modified = True
                                    break
                        if modified:
                            module_source = open(module_file_name + '.py', encoding = 'utf-8-sig').read()
                            imported_modules = []
                            prev_scope = scope
                            s = parse_and_to_str(tokenizer.tokenize(module_source), module_source, module_file_name + '.py', imported_modules)
                            modules[module_name] = Module(scope)
                            open(module_file_name + '.11l', 'w', encoding = 'utf-8', newline = "\n").write(s)
                            open(module_file_name + '.py_global_scope', 'w', encoding = 'utf-8', newline = "\n").write(eldf.to_eldf(scope.serialize_to_dict(imported_modules)))
                            scope = prev_scope
                            if this_node.imported_modules is not None:
                                this_node.imported_modules.extend(imported_modules)
                        else:
                            module_scope = Scope(None)
                            module_scope.deserialize_from_dict(py_global_scope)
                            modules[module_name] = Module(module_scope)
                            if this_node.imported_modules is not None:
                                this_node.imported_modules.extend(py_imported_modules)

                    if '.' in module_name:
                        scope.add_var(module_name.split('.')[0], True, '(Module)')
                    scope.add_var(module_name, True, '(Module)')
                    next_token()
                    if token.value(source) != ',':
                        break
                    next_token()

                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'from':
                next_token()
                assert(token.value(source) in ('typing', 'functools', 'itertools', 'enum', 'copy'))
                next_token()
                advance('import')
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected name', token)
                    next_token()
                    if token.value(source) != ',':
                        break
                    next_token()

                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()
                continue

            elif token.value(source) == 'def':
                node = ASTFunctionDefinition()
                node.function_name = expected_name('function name')
                scope.add_var(node.function_name, True, node = node)

                if token.value(source) != '(': # )
                    raise Error('expected `(` after function name', token) # )(

                next_token()
                was_default_argument = False

                def advance_type():
                    type_ = token.value(source)
                    next_token()
                    if token.value(source) == '[': # ]
                        nesting_level = 0
                        while True:
                            type_ += token.value(source)
                            if token.value(source) == '[':
                                next_token()
                                nesting_level += 1
                            elif token.value(source) == ']':
                                next_token()
                                nesting_level -= 1
                                if nesting_level == 0:
                                    break
                            elif token.value(source) == ',':
                                type_ += ' '
                                next_token()
                            else:
                                if token.category != Token.Category.NAME:
                                    raise Error('expected subtype name', token)
                                next_token()
                    return type_

                while token.value(source) != ')':
                    if token.value(source) == '*':
                        assert(node.first_named_only_argument is None)
                        node.first_named_only_argument = len(node.function_arguments)
                        next_token()
                        advance(',')
                        continue
                    if token.category != Token.Category.NAME:
                        raise Error('expected function\'s argument name', token)
                    func_arg_name = tokensn.token_str()
                    next_token()
                    type_ = ''
                    qualifier = ''
                    if token.value(source) == ':': # this is a type hint
                        next_token()
                        if token.category == Token.Category.STRING_LITERAL:
                            type_ = token.value(source)[1:-1]
                            if token.value(source)[0] == '"': # `def insert(i, n : "Node"):` -> `F insert(i, Node &n)`
                                qualifier = '&'
                            next_token()
                        else:
                            type_ = advance_type()
                            if type_ == 'list':
                                type_ = ''
                                qualifier = '&'

                    if token.value(source) == '=':
                        next_token()
                        expr = expression()
                        check_vars_defined(expr)
                        default = expr.to_str()
                        was_default_argument = True
                    else:
                        if was_default_argument and node.first_named_only_argument is None:
                            raise Error('non-default argument follows default argument', tokens[tokeni-1])
                        default = ''
                    node.function_arguments.append((func_arg_name, default, type_, qualifier)) # ((
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
                        node.function_return_type = advance_type()

                if source[token.end:token.end+7] == ' # -> &':
                    node.function_return_type += '&'
                elif source[token.end:token.end+8] == ' # const':
                    node.is_const = True

                node.parent = this_node
                new_scope(node, map(lambda arg: (arg[0], arg[2]), node.function_arguments))

                if len(node.children) == 0: # needed for:
                    n = ASTPass()           # class FileToStringProxy:
                    n.parent = node         #     def __init__(self):
                    node.children.append(n) #         self.result = []

                # Detect virtual functions and assign `virtual_category`
                if type(this_node) == ASTClassDefinition and node.function_name != '__init__':
                    if this_node.base_class_node is not None:
                        for child in this_node.base_class_node.children:
                            if type(child) == ASTFunctionDefinition and child.function_name == node.function_name:
                                if child.virtual_category == ASTFunctionDefinition.VirtualCategory.NO:
                                    if child.function_return_type == '':
                                        raise Error('please specify return type of virtual function', tokens[child.tokeni])
                                    if len(child.children) and type(child.children[0]) == ASTException and child.children[0].expression.symbol.id == '(' and child.children[0].expression.children[0].token.value(source) == 'NotImplementedError': # )
                                        child.virtual_category = ASTFunctionDefinition.VirtualCategory.ABSTRACT
                                    else:
                                        child.virtual_category = ASTFunctionDefinition.VirtualCategory.NEW
                                node.virtual_category = ASTFunctionDefinition.VirtualCategory.ASSIGN if child.virtual_category == ASTFunctionDefinition.VirtualCategory.ABSTRACT else ASTFunctionDefinition.VirtualCategory.OVERRIDE
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
                        if base_class is None:
                            raise Error('class `' + node.base_class_name + '` is not defined', tokens[tokeni-1])
                        if base_class.type != '(Class)':
                            raise Error('expected a class name', tokens[tokeni-1])
                        assert(type(base_class.node) == ASTClassDefinition)
                        node.base_class_node = base_class.node
                    expected(')')

                if source[token.end:token.end+4] == ' # &':
                    node.is_inout = True

                new_scope(node)

            elif token.value(source) == 'pass':
                node = ASTPass()
                next_token()
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
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
                    while token is not None and token.value(source) in ('elif', 'else'):
                        if token.value(source) == 'elif':
                            n.else_or_elif = ASTElseIf()
                            n.else_or_elif.parent = n
                            n = n.else_or_elif
                            next_token()
                            n.set_expression(expression())
                            new_scope(n)
                        if token is not None and token.value(source) == 'else':
                            n.else_or_elif = ASTElse()
                            n.else_or_elif.parent = n
                            next_token()
                            new_scope(n.else_or_elif)
                            break

            elif token.value(source) == 'while':
                node = ASTWhile()
                next_token()
                node.set_expression(expression())
                if node.expression.token.category in (Token.Category.CONSTANT, Token.Category.NUMERIC_LITERAL, Token.Category.STRING_LITERAL) and node.expression.token.value(source) != 'True':
                    raise Error('do you mean `while True`?', node.expression.token) # forbid `while 1:`
                new_scope(node)

            elif token.value(source) == 'for':
                node = ASTFor()
                next_token()
                prev_scope = scope
                scope = Scope(None)
                scope.parent = prev_scope

                node.loop_variables = [tokensn.token_str()]
                scope.add_var(node.loop_variables[0], True)
                next_token()
                while token.value(source) == ',':
                    next_token()
                    node.loop_variables.append(tokensn.token_str())
                    scope.add_var(tokensn.token_str(), True)
                    next_token()
                advance('in')
                node.set_expression(expression())
                new_scope(node)
                scope = prev_scope

                if token is not None and token.value(source) == 'else':
                    node.was_no_break = ASTNodeWithChildren()
                    node.was_no_break.parent = node
                    next_token()
                    new_scope(node.was_no_break)

            elif token.value(source) == 'continue':
                node = ASTContinue()
                next_token()
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'break':
                node = ASTBreak()
                next_token()
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'return':
                node = ASTReturn()
                next_token()
                if token.category in (Token.Category.DEDENT, Token.Category.STATEMENT_SEPARATOR):
                    node.expression = None
                else:
                    node.set_expression(expression())
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) in ('nonlocal', 'global'):
                nonlocal_or_global = token.value(source)
                next_token()
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected ' + nonlocal_or_global + ' variable name', token)
                    if nonlocal_or_global == 'nonlocal':
                        if source[token.end + 1 : token.end + 5] == "# =\n":
                            scope.nonlocals_copy.add(token.value(source))
                        else:
                            scope.nonlocals.add(token.value(source))
                    else:
                        scope.globals.add(token.value(source))
                    next_token()
                    if token.value(source) == ',':
                        next_token()
                    else:
                        break
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()
                continue

            elif token.value(source) == 'assert':
                node = ASTAssert()
                next_token()
                node.set_expression(expression())
                if token.value(source) == ',':
                    next_token()
                    node.set_expression2(expression())
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            elif token.value(source) == 'raise':
                node = ASTException()
                next_token()
                node.set_expression(expression())
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
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

                if peek_token().value(source) != ':':
                    node.exception_object_type = expected_name('exception object type name')
                    while token.value(source) == '.':
                        node.exception_object_type += ':' + expected_name('type name')

                    if node.exception_object_type.startswith('self:'):
                        node.exception_object_type = '.' + node.exception_object_type[5:]

                    if token.value(source) != ':':
                        advance('as')
                        if token.category != Token.Category.NAME:
                            raise Error('expected exception object name', token)
                        node.exception_object_name = tokensn.token_str()
                        scope.add_var(node.exception_object_name, True)
                        next_token()
                else:
                    next_token()
                    node.exception_object_type = ''

                new_scope(node)
                scope = prev_scope

            elif token.value(source) == 'del':
                node = ASTDel()
                next_token()
                node.set_expression(expression())
                if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            else:
                raise Error('unrecognized statement started with keyword', token)

        elif token.category == Token.Category.NAME and peek_token().value(source) == '=':
            name_token = token
            name_token_str = tokensn.token_str()
            node = ASTExprAssignment()
            node.set_dest_expression(tokensn)
            next_token()
            next_token()
            node.set_expression(expression())
            if node.expression.symbol.id == '.' and len(node.expression.children) == 2 and node.expression.children[1].token_str().isupper(): # replace `category = Token.Category.NAME` with `category = NAME`
                node.set_expression(node.expression.children[1])
                node.expression.parent = None
                node.expression.skip_find_and_get_prefix = True # this can not be replaced with `isupper()` check before `find_and_get_prefix()` call because there will be conflict with uppercase [constant] variables, like `WIDTH` or `HEIGHT` (they[‘variables’] will not be checked, but they should)
            type_name = ''
            if node.expression.token.category == Token.Category.STRING_LITERAL or (node.expression.function_call and node.expression.children[0].token_str() == 'str') \
                or (node.expression.symbol.id == '+' and len(node.expression.children) == 2 and (node.expression.children[0].token.category == Token.Category.STRING_LITERAL
                                                                                              or node.expression.children[1].token.category == Token.Category.STRING_LITERAL)):
                type_name = 'str'
            elif node.expression.var_type() == 'List':
                type_name = 'List'
            elif node.expression.is_dict():
                type_name = 'Dict'
            elif node.expression.function_call and node.expression.children[0].symbol.id == '.' and \
                 node.expression.children[0].children[0].token_str() == 'collections' and \
                 node.expression.children[0].children[1].token_str() == 'defaultdict':
                type_name = 'DefaultDict'
            node.add_vars = [scope.add_var(name_token_str, False, type_name, name_token)]
            if node.expression.symbol.id == '[' and len(node.expression.children) == 0: # ]
                if node.add_vars[0]:
                    raise Error('please specify type of empty list', Token(node.dest_expression.token.start, node.expression.token.end + 1, Token.Category.NAME))
                node.drop_list = True
            if not (token is None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)): # `poss_nbors = (x-1,y),(x-1,y+1)`
                raise Error('expected end of statement', token)                                                      #                      ^
            if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()
            if ((node.dest_expression.token_str() == 'Char'   and node.expression.token_str() == 'str')   # skip `Char = str` statement
             or (node.dest_expression.token_str() == 'Byte'   and node.expression.token_str() == 'int')   # skip `Byte = int` statement
             or (node.dest_expression.token_str() == 'Int64'  and node.expression.token_str() == 'int')   # skip `Int64 = int` statement
             or (node.dest_expression.token_str() == 'UInt64' and node.expression.token_str() == 'int')   # skip `UInt64 = int` statement
             or (node.dest_expression.token_str() == 'UInt32' and node.expression.token_str() == 'int')): # skip `UInt32 = int` statement
                continue

        elif token.category == Token.Category.NAME and (peek_token().value(source) == ':' # this is type hint
                  or (token.value(source) == 'self' and peek_token().value(source) == '.' and peek_token(2).category == Token.Category.NAME)
                                                   and peek_token(3).value(source) == ':'):
            is_self = peek_token().value(source) == '.'
            if is_self:
                if not (type(this_node) == ASTFunctionDefinition and this_node.function_name == '__init__'):
                    raise Error('type annotation for `self.*` is permitted only inside `__init__`', token)
                next_token()
                next_token()
            name_token = token
            var = tokensn.token_str()
            next_token()
            advance(':')
            if token.category not in (Token.Category.NAME, Token.Category.STRING_LITERAL):
                raise Error('expected type name', token)
            type_ = token.value(source) if token.category == Token.Category.NAME else token.value(source)[1:-1]
            type_token = token
            next_token()
            while token.value(source) == '.': # for `category : Token.Category`
                type_ += '.' + expected_name('type name')
            if is_self:
                scope.parent.add_var(var, True, type_, name_token)
            else:
                scope.add_var(var, True, type_, name_token)
            type_args = []
            if token.value(source) == '[':
                next_token()
                while token.value(source) != ']':
                    if token.value(source) == '[': # for `Callable[[str, int], str]`
                        next_token()
                        if token.value(source) == ']': # for `Callable[[], str]`
                            type_arg = ''
                        else:
                            type_arg = token.value(source)
                            next_token()
                            while token.value(source) == ',':
                                next_token()
                                type_arg += ',' + token.value(source)
                                next_token() # [
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
                            elif token.value(source) == ',':
                                type_arg += ' '
                                next_token()
                            else:
                                assert(token.category == Token.Category.NAME)
                                next_token()
                        type_args.append(type_arg)
                    else:
                        type_args.append(token.value(source))
                        next_token()
                    while token.value(source) == '.': # for `datetime.date` in `dates : List[datetime.date] = []`
                        type_args[-1] += '.' + expected_name('subtype name') # [[
                    if token.value(source) not in ',]':
                        raise Error('expected `,` or `]` in type\'s arguments list', token)
                    if token.value(source) == ',':
                        next_token()
                next_token()

            if token is not None and token.value(source) == '=':
                node = ASTAssignmentWithTypeHint()
                next_token()
                node.set_expression(expression())
            else:
                node = ASTTypeHint()
                if source[tokens[tokeni-1].end:tokens[tokeni-1].end+4] == ' # &':
                    node.is_reference = True
                if not (token is None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)):
                    raise Error('expected end of statement', token)
            node.type_token = type_token
            node.var = var
            node.type = type_
            node.type_args = type_args

            assert(token is None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

            if is_self:
                node.parent = this_node.parent
                this_node.parent.children.append(node)
                node.walk_expressions(check_vars_defined)
                continue

        elif token.category == Token.Category.DEDENT:
            next_token()
            if token.category == Token.Category.STATEMENT_SEPARATOR: # Token.Category.EOF
                next_token()
                assert(token is None)
            return

        else:
            node_expression = expression()
            if token is not None and token.value(source) == '=':
                node = ASTExprAssignment()
                if node_expression.token.category == Token.Category.NAME:
                    assert(False) #node.add_var = scope.add_var(node_expression.token.value(source))
                if node_expression.tuple:
                    node.add_vars = []
                    for v in node_expression.children:
                        if v.token.category != Token.Category.NAME:
                            node.is_tuple_assign_expression = True
                            break
                        node.add_vars.append(scope.add_var(v.token_str()))
                else:
                    node.add_vars = [False]
                node.set_dest_expression(node_expression)
                next_token()
                while True:
                    expr = expression()
                    if token is not None and token.value(source) == '=':
                        expr.ast_parent = node
                        node.additional_dest_expressions.append(expr)
                        next_token()
                    else:
                        node.set_expression(expr)
                        break
            else:
                node = ASTExpression()
                node.set_expression(node_expression)
                if not (token is None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)):
                    raise Error('expected end of statement', token)
            if not (token is None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)): # `(w, h) = int(w1), int(h1)`
                raise Error('expected end of statement', token)                                                      #                  ^
            if token is not None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

            if (type(node) == ASTExprAssignment and node_expression.token_str() == '.' and node_expression.children[0].token_str() == 'self'
                    and type(this_node) == ASTFunctionDefinition and this_node.function_name == '__init__'): # only in constructors

                assert(type(this_node.parent) == ASTClassDefinition)
                found_in_base_class = False
                if this_node.parent.base_class_node is not None:
                    found_in_base_class = this_node.parent.base_class_node.find_member_including_base_classes(node_expression.children[1].token_str())

                if not found_in_base_class and scope.parent.add_var(node_expression.children[1].token_str()):
                    if node.expression.symbol.id == '[' and len(node.expression.children) == 0: # ]
                        raise Error('please specify type of empty list', Token(node.dest_expression.leftmost(), node.expression.rightmost(), Token.Category.NAME))
                    node.add_vars = [True]
                    node.set_dest_expression(node_expression.children[1])
                    node.parent = this_node.parent
                    this_node.parent.children.append(node)
                    node.walk_expressions(check_vars_defined)
                    continue
                elif ((node.expression.symbol.id == '[' and len(node.expression.children) == 0) # ] # skip `self.* = []` because `create_array({})` is meaningless
                   or (node.expression.symbol.id == '(' and len(node.expression.children) == 1 and node.expression.children[0].token_str() == 'set')): # ) # skip `self.* = set()`
                    continue

        node.walk_expressions(check_vars_defined)

        node.parent = this_node
        this_node.children.append(node)

        if one_line_scope and tokens[tokeni-1].value(source) != ';':
            return

    return

tokens    = []
source    = ''
tokeni    = -1
token     = Token(0, 0, Token.Category.STATEMENT_SEPARATOR)
scope     = Scope(None)
tokensn   = SymbolNode(token)
file_name = ''

def parse_and_to_str(tokens_, source_, file_name_, imported_modules = None):
    if len(tokens_) == 0: return ASTProgram().to_str()
    global tokens, source, tokeni, token, scope, tokensn, file_name
    prev_tokens    = tokens
    prev_source    = source
    prev_tokeni    = tokeni
    prev_token     = token
#   prev_scope     = scope
    prev_tokensn   = tokensn
    prev_file_name = file_name
    tokens = tokens_ + [Token(len(source_), len(source_), Token.Category.STATEMENT_SEPARATOR)]
    source = source_
    tokeni = -1
    token = None
    scope = Scope(None)
    for pytype in python_types_to_11l:
        scope.add_var(pytype)
    scope.add_var('IntEnum', True, '(Class)', node = ASTClassDefinition())
    file_name = file_name_
    next_token()
    p = ASTProgram()
    p.imported_modules = imported_modules
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
                if child is not None:
                    f(child)

        node.walk_expressions(f)
        node.walk_children(check_for_and_or)
    check_for_and_or(p)

    def transformations(node):
        if isinstance(node, ASTNodeWithChildren):
            index = 0
            while index < len(node.children):
                child = node.children[index]
                if index < len(node.children) - 1 and type(child) == ASTExprAssignment and child.dest_expression.token.category == Token.Category.NAME and type(node.children[index+1]) == ASTIf and type(node.children[index+1].else_or_elif) == ASTElseIf: # transform if-elif-else chain into switch
                    if_node = node.children[index+1]
                    var_name = child.dest_expression.token.value(source)
                    transformation_possible = True
                    while True:
                        if not (if_node.expression.symbol.id == '==' and if_node.expression.children[0].token.category == Token.Category.NAME and if_node.expression.children[0].token.value(source) == var_name
                                                                     and if_node.expression.children[1].token.category in (Token.Category.STRING_LITERAL, Token.Category.NUMERIC_LITERAL)):
                            transformation_possible = False
                            break
                        if_node = if_node.else_or_elif
                        if if_node is None or type(if_node) == ASTElse:
                            break

                    if transformation_possible:
                        tid = child.dest_expression.scope.find(var_name)
                        assert(tid is not None)
                        found_reference_to_var_name = False
                        def find_reference_to_var_name(node):
                            def f(e : SymbolNode):
                                if e.token.category == Token.Category.NAME and e.token_str() == var_name and id(e.scope.find(var_name)) == id(tid):
                                    nonlocal found_reference_to_var_name
                                    found_reference_to_var_name = True
                                    return
                                for child in e.children:
                                    if child is not None:
                                        f(child)
                            node.walk_expressions(f)
                            node.walk_children(find_reference_to_var_name)
                        if_node = node.children[index+1]
                        while True:
                            if_node.walk_children(find_reference_to_var_name) # looking for switch variable inside switch statements
                            if found_reference_to_var_name:
                                break
                            if type(if_node) == ASTElse:
                                break
                            if_node = if_node.else_or_elif
                            if if_node is None:
                                break
                        if not found_reference_to_var_name:
                            i = index + 2
                            while i < len(node.children):
                                find_reference_to_var_name(node.children[i]) # looking for switch variable after switch
                                if found_reference_to_var_name:
                                    break
                                i += 1

                        switch_node = ASTSwitch()
                        switch_node.set_expression(child.dest_expression if found_reference_to_var_name else child.expression)
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
                            if if_node is None:
                                break
                        if found_reference_to_var_name:
                            index += 1
                        else:
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

                if type(child) == ASTFor:
                    if len(child.loop_variables): # detect loop variables' changing/modification, and add qualifier `=` to changing ones
                        lvars = child.loop_variables
                        found = set()
                        def detect_lvars_modification(node):
                            if type(node) == ASTExprAssignment:
                                nonlocal found
                                if node.dest_expression.token_str() in lvars:
                                    found.add(node.dest_expression.token_str())
                                    if len(lvars) == 1:
                                        return
                                elif node.dest_expression.tuple:
                                    for t in node.dest_expression.children:
                                        if t.token_str() in lvars:
                                            found.add(t.token_str())
                                            if len(lvars) == 1:
                                                return
                            def f(e : SymbolNode):
                                if e.symbol.id[-1] == '=' and e.symbol.id not in ('==', '!=', '<=', '>=') and e.children[0].token_str() in lvars: # +=, -=, *=, /=, etc.
                                    nonlocal found
                                    found.add(e.children[0].token_str())
                            node.walk_expressions(f)
                            node.walk_children(detect_lvars_modification)
                        detect_lvars_modification(child)
                        for lvar in found:
                            lvari = lvars.index(lvar)
                            child.loop_variables[lvari] = '=' + child.loop_variables[lvari]

                    if child.expression.symbol.id == '(' and child.expression.children[0].symbol.id == '.' \
                            and child.expression.children[0].children[0].token_str() == 'os' \
                            and child.expression.children[0].children[1].token_str() == 'walk': # ) # detect `for ... in os.walk(...)` and remove `dirs[:] = ...` statement
                        child.os_walk = True
                        assert(len(child.loop_variables) == 3)
                        c0 = child.children[0]
                        if (type(c0) == ASTExprAssignment and c0.dest_expression.symbol.id == '[' # ]
                                                          and len(c0.dest_expression.children) == 2
                                                          and c0.dest_expression.children[1] is None
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
                        set_index_node.set_dest_expression(SymbolNode(Token(0, 0, Token.Category.NAME), child.loop_variables[0].lstrip('=')))
                        child.loop_variables.pop(0)
                        start = ''
                        if len(child.expression.children) >= 5:
                            if child.expression.children[4] is not None:
                                assert(child.expression.children[3].to_str() == 'start')
                                start = child.expression.children[4].to_str()
                            else:
                                start = child.expression.children[3].to_str()
                        set_index_node.set_expression(SymbolNode(Token(0, 0, Token.Category.NAME), 'L.index' + (' + ' + start if start != '' else '')))
                        set_index_node.add_vars = [True]
                        set_index_node.parent = child
                        child.children.insert(0, set_index_node)
                        child.expression.children[0].parent = child.expression.parent
                        child.expression.children[0].ast_parent = child.expression.ast_parent
                        child.expression = child.expression.children[1]

                elif type(child) == ASTFunctionDefinition: # detect function's arguments changing/modification inside this function, and add qualifier `=` to changing ones
                    if len(child.function_arguments):
                        fargs = [farg[0] for farg in child.function_arguments]
                        found = set()
                        def detect_arguments_modification(node):
                            if type(node) == ASTExprAssignment:
                                nonlocal found
                                if node.dest_expression.token_str() in fargs:
                                    found.add(node.dest_expression.token_str())
                                    if len(fargs) == 1:
                                        return
                                elif node.dest_expression.tuple:
                                    for t in node.dest_expression.children:
                                        if t.token_str() in fargs:
                                            found.add(t.token_str())
                                            if len(fargs) == 1:
                                                return
                            def f(e : SymbolNode):
                                if e.symbol.id[-1] == '=' and e.symbol.id not in ('==', '!=', '<=', '>=') and e.children[0].token_str() in fargs: # +=, -=, *=, /=, etc.
                                    nonlocal found
                                    found.add(e.children[0].token_str())
                            node.walk_expressions(f)
                            node.walk_children(detect_arguments_modification)
                        detect_arguments_modification(child)
                        for farg in found:
                            fargi = fargs.index(farg)
                            if child.function_arguments[fargi][3] != '&': # if argument already has `&` qualifier, then qualifier `=` is not needed
                                child.function_arguments[fargi] = ('=' + child.function_arguments[fargi][0], child.function_arguments[fargi][1], child.function_arguments[fargi][2], child.function_arguments[fargi][3])

                index += 1

        node.walk_children(transformations)
    transformations(p)

    s = p.to_str() # call `to_str()` moved here [from outside] because it accesses global variables `source` (via `token.value(source)`) and `tokens` (via `tokens[ti]`)

    tokens    = prev_tokens
    source    = prev_source
    tokeni    = prev_tokeni
    token     = prev_token
#   scope     = prev_scope
    tokensn   = prev_tokensn
    file_name = prev_file_name

    return s
