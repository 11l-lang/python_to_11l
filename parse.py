try:
    from python_to_11l.tokenizer import Token
except ImportError:
    from tokenizer import Token
from typing import List, Tuple, Dict, Callable

class Scope:
    parent : 'Scope'
    class Var:
        type : str
        def __init__(self, type):
            self.type = type
    vars : Dict[str, Var]
    nonlocals : set
    is_function : bool

    def __init__(self, func_args):
        self.parent = None
        if func_args != None:
            self.is_function = True
            self.vars = dict(map(lambda x: (x, Scope.Var(None)), func_args))
        else:
            self.is_function = False
            self.vars = {}
        self.nonlocals = set()

    def add_var(self, name, error_if_already_defined = False, type = None, err_token = None):
        s = self
        while True:
            if name in s.nonlocals:
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
            self.vars[name] = Scope.Var(type)
            return True
        elif error_if_already_defined:
            raise Error('redefinition of already defined variable is not allowed', err_token if err_token != None else token)
        return False

    def find(self, name, token):
        if name == 'self':
            return ''
        if name in ('isinstance', 'len', 'super', 'print', 'ord', 'chr', 'range', 'zip', 'sum', 'open', 'min', 'max', 'hex'):
            return ''

        s = self
        while True:
            if name in s.nonlocals:
                return '@'
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
                    return ':' if capture_level > 0 else ''
                else:
                    return capture_level*'@'
            if s.is_function:
                capture_level += 1
            s = s.parent
            if s == None:
                raise Error('variable is not defined', token)

    def var_type(self, name):
        s = self
        while True:
            if name in s.vars:
                return s.vars[name].type
            s = s.parent
            if s == None:
                return None

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
            return self.scope_prefix + self.token.value(source)

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
            else:
                l = 3 if s[0:3] in ('"""', "'''") else 1
                if '\\' in s or ('‘' in s and not '’' in s) or (not '‘' in s and '’' in s):
                    s = s.replace("\n", "\\n\\\n").replace("\\\\n\\\n", "\\\n")
                    if s[0] == '"':
                        return s if l == 1 else s[2:-2]
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
                    if self.children[0].children[0].symbol.id == '{' and self.children[0].children[1].token.value(source) == 'get': # } # replace `{'and':'&', 'or':'|', 'in':'C'}.get(self.symbol.id, 'symbol-' + self.symbol.id)` with `(S .symbol.id {‘and’ {‘&’}; ‘or’ {‘|’}; ‘in’ {‘C’} E ‘symbol-’(.symbol.id)})`
                        parenthesis = ('(', ')') if self.parent != None else ('', '')
                        return parenthesis[0] + self.children[0].to_str() + parenthesis[1]
                    if self.children[0].children[1].token.value(source) == 'join': # replace `', '.join(arr)` with `arr.join(‘, ’)`
                        assert(len(self.children) == 3)
                        return (self.children[1].to_str() if self.children[1].token.category == Token.Category.NAME or self.children[1].symbol.id == 'for' else '(' + self.children[1].to_str() + ')') + '.join(' + self.children[0].children[0].to_str() + ')'
                    repl = {'startswith':'starts_with', 'endswith':'ends_with', 'find':'findi', 'rfind':'rfindi'}.get(self.children[0].children[1].token.value(source), '')
                    if repl != '': # replace `startswith` with `starts_with`, `endswith` with `ends_with`, etc.
                        #assert(len(self.children) == 3)
                        res = self.children[0].children[0].to_str() + '.' + repl + '('
                        for i in range(1, len(self.children), 2):
                            assert(self.children[i+1] == None)
                            res += self.children[i].to_str()
                            if i < len(self.children)-2:
                                res += ', '
                        return res + ')'

                func_name = self.children[0].to_str()
                if func_name == 'str':
                    func_name = 'String'
                elif func_name == 'int':
                    func_name = 'Int'
                elif func_name == 'open':
                    func_name = 'File'

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
                    return c0 + '[' + self.children[1].to_str() + ']'

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
            res = self.children[2].to_str()
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
                if self.children[0].to_str() == 'self':
                    parent = self
                    while parent.parent:
                        parent = parent.parent
                    ast_parent = parent.ast_parent
                    function_nesting = 0
                    while type(ast_parent) != ASTProgram:
                        if type(ast_parent) == ASTFunctionDefinition:
                            function_nesting += 1
                            if function_nesting == 2:
                                break
                        elif type(ast_parent) == ASTClassDefinition:
                            break
                        ast_parent = ast_parent.parent
                    return ('@' if function_nesting == 2 else '') + '.' + self.children[1].to_str()
                return self.children[0].to_str() + '.' + self.children[1].to_str()
            elif self.symbol.id == '+=' and self.children[1].symbol.id == '[' and self.children[1].is_list: # ]
                return self.children[0].to_str() + ' [+]= ' + (self.children[1].to_str()[1:-1] if len(self.children[1].children) == 1 else self.children[1].to_str())
            elif self.symbol.id == '+=' and self.children[1].token.value(source) == '1':
                return self.children[0].to_str() + '++'
            elif self.symbol.id == '-=' and self.children[1].token.value(source) == '1':
                return '--' + self.children[0].to_str()
            elif self.symbol.id == '+=' and self.children[0].token.category == Token.Category.NAME and self.children[0].var_type() == 'str':
                return self.children[0].to_str() + ' ‘’= ' + self.children[1].to_str()
            elif self.symbol.id == '+' and self.children[1].symbol.id == '*' and self.children[1].children[1].token.category == Token.Category.STRING_LITERAL: # for `outfile.write('<blockquote'+(ch=='<')*' class="re"'+'>')`
                return self.children[0].to_str() + '(' + self.children[1].to_str() + ')'
            elif self.symbol.id == '+' and self.children[1].symbol.id == '*' and self.children[1].children[0].token.category == Token.Category.STRING_LITERAL: # for `outfile.write("<table"+' style="display: inline"'*(prevci != 0 and instr[prevci-1] != "\n")+...)`
                return self.children[0].to_str() + '(' + self.children[1].to_str() + ')'
            elif self.symbol.id == '+' and self.children[0].symbol.id == '+' and self.children[1].token.category == Token.Category.STRING_LITERAL \
                                                                             and self.children[0].children[1].token.category == Token.Category.STRING_LITERAL: # for `outfile.write(... + '<br /></span>' # ... \n + '<div class="spoiler_text" ...)`
                return self.children[0].to_str() + '""' + self.children[1].to_str()
            elif self.symbol.id == '+' and (self.children[0].token.category == Token.Category.STRING_LITERAL
                                         or self.children[1].token.category == Token.Category.STRING_LITERAL
                                         or (self.children[0].symbol.id == '+' and self.children[0].children[1].token.category == Token.Category.STRING_LITERAL)):
                c1 = self.children[1].to_str()
                return self.children[0].to_str() + ('(' + c1 + ')' if c1[0] == '.' else c1)
            elif self.symbol.id == '+' and self.children[1].symbol.id == '*' and self.children[1].children[0].token.category == Token.Category.STRING_LITERAL: # for `self.newlines() + ' ' * (indent*3) + 'F ' + ...`
                return self.children[0].to_str() + '‘’(' + self.children[1].to_str() + ')'
            elif self.symbol.id == '+' and self.children[0].symbol.id == '*' and self.children[0].children[0].token.category == Token.Category.STRING_LITERAL: # for `' ' * (indent*3) + self.expression.to_str() + "\n"`
                c1 = self.children[1].to_str()
                return '(' + self.children[0].to_str() + ')‘’' + ('(' + c1 + ')' if c1[0] == '.' else c1)
            elif self.symbol.id == '+' and (self.children[0].var_type() == 'str' or self.children[1].var_type() == 'str'):
                return self.children[0].to_str() + '‘’' + self.children[1].to_str()
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
            else:
                return self.children[0].to_str() + ' ' + {'and':'&', 'or':'|', 'in':'C', '//':'I/', '^':'(+)'}.get(self.symbol.id, self.symbol.id) + ' ' + self.children[1].to_str()
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
        return ' ' * (indent*3) + ('A ' if self.add_var else '') + self.dest_expression.to_str() + ' = ' + self.expression.to_str() + "\n"

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

python_types_to_11l = {'int':'Int', 'str':'String', 'bool':'Bool', 'None':'N', 'List':'Array', 'Tuple':'Tuple', 'Dict':'Dict', 'List[List[str]]':'Array[Array[String]]', 'List[str]':'Array[String]'}

class ASTTypeHint(ASTNode):
    var : str
    type : str
    type_args : List[str]

    def to_str(self, indent):
        if self.type == 'Callable':
            return ' ' * (indent*3) + '(' + ', '.join(python_types_to_11l[ty] for ty in self.type_args[0].split(',')) + ' -> ' + python_types_to_11l[self.type_args[1]] + ') ' + self.var + "\n"
        return ' ' * (indent*3) + python_types_to_11l[self.type] + ('[' + ', '.join(python_types_to_11l[ty] for ty in self.type_args) + ']' if len(self.type_args) else '') + ' ' + self.var + "\n"

class ASTAssignmentWithTypeHint(ASTTypeHint, ASTNodeWithExpression):
    def to_str(self, indent):
        expression_str = self.expression.to_str()
        return super().to_str(indent)[:-1] + (' = ' + expression_str if expression_str not in ('[]', 'Dict()') else '') + "\n"

class ASTFunctionDefinition(ASTNodeWithChildren):
    function_name : str
    function_return_type : str = ''
    function_arguments : List[Tuple[str, SymbolNode, str]]# = []
    first_named_only_argument = None

    def __init__(self):
        super().__init__()
        self.function_arguments = []

    def to_str(self, indent):
        fargs = []
        for arg in self.function_arguments:
            farg = ''
            default_value = ''
            if arg[1] != None:
                default_value = arg[1].to_str()
            if arg[2] != '':
                farg += arg[2]
                if default_value == 'N':
                    farg += '?'
                farg += ' '
            farg += arg[0] + ('' if default_value == '' else ' = ' + default_value)
            fargs.append(farg)
        if self.first_named_only_argument != None:
            fargs.insert(self.first_named_only_argument, "'")
        if len(self.function_arguments) and self.function_arguments[0][0] == 'self':
            fargs.pop(0)
        return self.children_to_str(indent, 'F ' + (self.function_name if self.function_name != '__init__' else '')
            + '(' + ", ".join(fargs) + ')'
            + ('' if self.function_return_type == '' else ' -> ' + python_types_to_11l[self.function_return_type]))

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
            for child in case.children:
                f(child)

    def to_str(self, indent):
        r = ' ' * (indent*3) + 'S ' + self.expression.to_str() + "\n"
        for case in self.cases:
            r += case.children_to_str(indent + 1, 'E' if case.expression.token_str() == 'E' else case.expression.to_str())
        return r

class ASTWhile(ASTNodeWithChildren, ASTNodeWithExpression):
    def to_str(self, indent):
        return self.children_to_str(indent, 'L' if self.expression.token.category == Token.Category.CONSTANT and self.expression.token.value(source) == 'True' else 'L ' + self.expression.to_str())

class ASTFor(ASTNodeWithChildren, ASTNodeWithExpression):
    loop_variables : List[str]

    def to_str(self, indent):
        if len(self.loop_variables) == 1:
            return self.children_to_str(indent, 'L(' + self.loop_variables[0] + ') ' + self.expression.to_str())
        else:
            r = 'L(' + ''.join(self.loop_variables) + ') ' + self.expression.to_str()
            for index, loop_var in enumerate(self.loop_variables):
                r += "\n" + ' ' * ((indent+1)*3) + 'A ' + loop_var + ' = ' + ''.join(self.loop_variables) + '[' + str(index) + ']'
            return self.children_to_str(indent, r)

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
            next_token()
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
    def set_scope_recursive(sn, scope):
        sn.scope = scope
        for child in sn.children:
            if child != None:
                set_scope_recursive(child, scope)
    set_scope_recursive(left, scope)
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

def parse_internal(this_node):
    global token

    def new_scope(node, func_args = None):
        if token.value(source) != ':':
            raise Error('expected `:`', Token(tokens[tokeni-1].end, tokens[tokeni-1].end, tokens[tokeni-1].category))
        next_token()
        assert(token.category == Token.Category.INDENT) # error message ‘expected an indented block’ is already generated by tokenizer, so there is just an assert
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
                    node.modules.append(token.value(source))
                    scope.add_var(token.value(source), True)
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
                        type_ = expression().to_str()
                        type_ = {'IO[str]': 'File'}.get(type_, type_)
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
                    node.function_return_type = token.value(source)
                    next_token()

                new_scope(node, map(lambda arg: arg[0], node.function_arguments))

                if len(node.children) == 0: # needed for:
                    n = ASTPass()           # class FileToStringProxy:
                    n.parent = node         #     def __init__(self):
                    node.children.append(n) #         self.result = []

            elif token.value(source) == 'class':
                node = ASTClassDefinition()
                node.class_name = expected_name('class name')
                scope.add_var(node.class_name, True)

                if token.value(source) == '(':
                    node.base_class_name = expected_name('base class name')
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

            elif token.value(source) == 'nonlocal':
                next_token()
                while True:
                    if token.category != Token.Category.NAME:
                        raise Error('expected nonlocal variable name', token)
                    scope.nonlocals.add(token.value(source))
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
            node.add_var = scope.add_var(name_token.value(source), False, 'str' if node.expression.token.category == Token.Category.STRING_LITERAL else None, name_token)
            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

        elif token.category == Token.Category.NAME and peek_token().value(source) == ':': # this is type hint
            name_token = token
            var = token.value(source)
            next_token()
            type_ = expected_name('type name')
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
                if not (token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)):
                    raise Error('expected end of statement', token)
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
                    sn.scope_prefix = sn.scope.find(sn.token.value(source), sn.token)
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

    return

def parse(tokens_, source_):
    global tokens, source, tokeni, token, scope
    tokens = tokens_ + [Token(len(source_), len(source_), Token.Category.STATEMENT_SEPARATOR)]
    source = source_
    tokeni = -1
    token = None
    scope = Scope(None)
    for pytype in python_types_to_11l:
        scope.add_var(pytype)
    next_token()
    p = ASTProgram()
    if len(tokens_) == 0: return p
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
                raise Error("relative precedence of operators `and` and `or` is undetermined; please add parentheses this way:\n`(" \
                    + source[start:midend  ] + ')' + source[midend  :end] + "`\nor this way:\n`" \
                    + source[start:midstart] + '(' + source[midstart:end] + ')`', Token(start, end, Token.Category.OPERATOR_OR_DELIMITER))
            for child in e.children:
                if child != None:
                    f(child)

        node.walk_expressions(f)
        node.walk_children(check_for_and_or)
    check_for_and_or(p)

    def transformations(node):
        if type(node) == ASTSwitch:
            for case in node.cases:
                transformations(case)
        elif isinstance(node, ASTNodeWithChildren):
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
                        continue # to update child = node.children[index]

                if index < len(node.children) - 1 and type(child) == ASTExpression and child.expression.symbol.id == '-=' and child.expression.children[1].token.value(source) == '1' \
                        and type(node.children[index+1]) == ASTIf and len(node.children[index+1].expression.children) == 2 \
                        and node.children[index+1].expression.children[0].token.value(source) == child.expression.children[0].token.value(source): # transform `nesting_level -= 1 \n if nesting_level == 0:` into `if --nesting_level == 0`
                    node.children[index+1].expression.children[0] = child.expression
                    node.children.pop(index)
                    continue

                if type(child) == ASTFunctionDefinition: # detect function's arguments changing/modification inside this function, and add qualifier `=` to changing ones
                    for fargi in range(len(child.function_arguments)):
                        farg = child.function_arguments[fargi][0]
                        found = False
                        def detect_argument_modification(node):
                            if type(node) == ASTExprAssignment and node.dest_expression.to_str() == farg:
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

                transformations(child)
                index += 1
    transformations(p)

    return p
