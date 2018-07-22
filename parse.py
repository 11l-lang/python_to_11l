from tokenizer import Token
from typing import List, Dict, Callable

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
        def nud(s): raise Error('unknown unary operator', s.token.start)
        self.nud = nud
        def led(s, l): raise Error('unknown binary operator', s.token.start)
        self.led = led

class SymbolNode:
    token : Token
    symbol : SymbolBase
    children : List['SymbolNode']# = []
    function_call : bool
    tuple : bool
    is_list : bool

    def __init__(self, token):
        self.token = token
        self.symbol = None
        self.children = []
        self.function_call = False
        self.tuple = False
        self.is_list = False

    def to_str(self):
        # r = ''
        # prev_token_end = self.children[0].token.start
        # for c in self.children:
        #     r += source[prev_token_end:c.token.start]
        #     if c.token.value(source) != 'self': # hack for a while
        #         r += c.token.value(source)
        #     prev_token_end = c.token.end
        # return r
        if self.token.is_literal() or self.token.category == Token.Category.NAME:
            return self.token.value(source) if self.token.value(source) != 'self' else '' # hack for a while

        if self.symbol.id == '(': # )
            if self.function_call:
                func_name = self.children[0].token.value(source)
                if func_name == 'len': # replace `len(container)` with `container.len`
                    assert(len(self.children) == 2)
                    return self.children[1].to_str() + '.len'
                else:
                    res = func_name + '('
                    for i in range(1, len(self.children)):
                        res += self.children[i].to_str()
                        if i < len(self.children)-1:
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
                res = '['
                for i in range(len(self.children)):
                    res += self.children[i].to_str()
                    if i < len(self.children)-1:
                        res += ', '
                return res + ']'
            else:
                return self.children[0].to_str() + '[' + self.children[1].to_str() + ']'

        if len(self.children) == 1:
            #return '(' + self.symbol.id + self.children[0].to_str() + ')'
            return self.symbol.id + self.children[0].to_str()
        elif len(self.children) == 2:
            #return '(' + self.children[0].to_str() + self.symbol.id + self.children[1].to_str() + ')'
            if self.symbol.id == '.':
                return self.children[0].to_str() + self.symbol.id + self.children[1].to_str()
            else:
                return self.children[0].to_str() + ' ' + {'and':'&', 'or':'|'}.get(self.symbol.id, self.symbol.id) + ' ' + self.children[1].to_str()
        elif len(self.children) == 3:
            assert(self.symbol.id == 'if')
            return 'I ' + self.children[1].to_str() + ' {' + self.children[0].to_str() + '} E ' + self.children[2].to_str()

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
    def walk_expressions(self, f):
        pass

class ASTNodeWithChildren(ASTNode):
    # children : List['ASTNode'] = [] # OMFG! This actually means static (common for all objects of type ASTNode) variable, not default value of member variable, that was unexpected to me as it contradicts C++11 behavior
    children : List['ASTNode']
    tokeni : int

    def __init__(self):
        self.children = []
        self.tokeni = tokeni

    def children_to_str(self, indent, r):
        r = ('' if self.tokeni == 0 else (source[tokens[self.tokeni-2].end:tokens[self.tokeni].start].count("\n")-1) * "\n") + ' ' * (indent*3) + r + "\n"
        for c in self.children:
            r += c.to_str(indent+1)
        return r

class ASTProgram(ASTNodeWithChildren):
    def to_str(self):
        r = ''
        for c in self.children:
            r += c.to_str(0)
        return r

class ASTExpression(ASTNode):
    expression : SymbolNode

    def to_str(self, indent):
        return ' ' * (indent*3) + self.expression.to_str() + "\n"

    def walk_expressions(self, f):
        f(self.expression)

class ASTAssignment(ASTNode):
    dest : Token
    expression : SymbolNode

    def to_str(self, indent):
        return ' ' * (indent*3) + self.dest.value(source) + ' = ' + self.expression.to_str() + "\n"

    def walk_expressions(self, f):
        f(self.expression)

class ASTExprAssignment(ASTNode):
    dest_expression : SymbolNode
    expression : SymbolNode

    def to_str(self, indent):
        return ' ' * (indent*3) + self.dest_expression.to_str() + ' = ' + self.expression.to_str() + "\n"

    def walk_expressions(self, f):
        f(self.dest_expression)
        f(self.expression)

python_types_to_11l = {'int':'Int', 'str':'String', 'List':'Array', 'Tuple':'Tuple'}

class ASTTypeHint(ASTNode):
    var : str
    type : str
    type_args : List[str]

    def to_str(self, indent):
        return ' ' * (indent*3) + python_types_to_11l[self.type] + ('[' + ', '.join(python_types_to_11l[ty] for ty in self.type_args) + ']' if len(self.type_args) else '') + ' ' + self.var + "\n"

class ASTAssignmentWithTypeHint(ASTTypeHint):
    expression : SymbolNode

    def to_str(self, indent):
        return super().to_str(indent)[:-1] + ' = ' + self.expression.to_str() + "\n"

    def walk_expressions(self, f):
        f(self.expression)

class ASTFunctionDefinition(ASTNodeWithChildren):
    function_name : str
    function_arguments : List[str]# = []

    def __init__(self):
        super().__init__()
        self.function_arguments = []

    def to_str(self, indent):
        return self.children_to_str(indent, 'F ' + (self.function_name if self.function_name != '__init__' else '') \
            + '(' + ", ".join(self.function_arguments if len(self.function_arguments) == 0 or self.function_arguments[0] != 'self' else self.function_arguments[1:]) + ')')

class ASTIf(ASTNodeWithChildren):
    expression : SymbolNode

    def to_str(self, indent):
        return self.children_to_str(indent, 'I ' + self.expression.to_str())

    def walk_expressions(self, f):
        f(self.expression)

class ASTReturn(ASTNode):
    expression : SymbolNode

    def to_str(self, indent):
        return ' ' * (indent*3) + 'R' + (' ' + self.expression.to_str() if self.expression != None else '') + "\n"

    def walk_expressions(self, f):
        if self.expression != None: f(self.expression)

class ASTClassDefinition(ASTNodeWithChildren):
    base_class_name : str = None
    class_name : str

    def to_str(self, indent):
        return self.children_to_str(indent, 'T ' + self.class_name + ('(' + self.base_class_name + ')' if self.base_class_name and self.base_class_name != 'Exception' else ''))

class Error(Exception):
    def __init__(self, message, pos):
        self.message = message
        self.pos = pos

def next_token():
    global token, tokeni, tokensn
    if token == None and tokeni != -1:
        raise Error('no more tokens', len(source))
    tokeni += 1
    if tokeni == len(tokens):
        token = None
        tokensn = None
    else:
        token = tokens[tokeni]
        tokensn = SymbolNode(token)
        if token.category != Token.Category.INDENT:
            if token.category != Token.Category.KEYWORD or token.value(source) in allowed_keywords_in_expressions:
                tokensn.symbol = symbol_table["(literal)" if token.is_literal() else "(name)" if token.category == Token.Category.NAME else ';' if token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT) else token.value(source)]

def advance(value):
    if token.value(source) != value:
        raise Error('expected ' + value, token.start)
    next_token()

def peek_token(how_much = 1):
    return tokens[tokeni+how_much] if tokeni+how_much < len(tokens) else Token()

# This implementation is based on [http://svn.effbot.org/public/stuff/sandbox/topdown/tdop-4.py]
def expression(rbp = 0):
    def check_tokensn():
        if tokensn.symbol == None:
            raise Error('no symbol corresponding to token `' + token.value(source) + '` (belonging to ' + str(token.category) +') found while parsing expression', token.start)
    check_tokensn()
    t = tokensn
    next_token()
    check_tokensn()
    left = t.symbol.nud(t)
    while rbp < tokensn.symbol.lbp:
        t = tokensn
        next_token()
        left = t.symbol.led(t, left)
    return left

def infix(id, bp):
    def led(self, left):
        self.children.append(left)
        self.children.append(expression(self.symbol.led_bp))
        return self
    symbol(id, bp).set_led_bp(bp, led)

def infix_r(id, bp):
    def led(self, left):
        self.children.append(left)
        self.children.append(expression(self.symbol.led_bp - 1))
        return self
    symbol(id, bp).set_led_bp(bp, led)

def prefix(id, bp):
    def nud(self):
        self.children.append(expression(self.symbol.nud_bp))
        return self
    symbol(id, bp).set_nud_bp(bp, nud)

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

symbol("(name)").nud = lambda self: self
symbol("(literal)").nud = lambda self: self

#symbol("(end)")
symbol(';')
symbol(',')

def led(self, left):
    if token.category != Token.Category.NAME:
        raise Error('expected an attribute name', token.start)
    self.children.append(left)
    self.children.append(tokensn)
    next_token()
    return self
symbol('.').led = led

def led(self, left):
    self.function_call = True
    self.children.append(left) # (
    if token.value(source) != ')':
        while True:
            self.children.append(expression())
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
            self.children.append(expression())
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
    self.children.append(left)
    self.children.append(expression())
    advance(']')
    return self
symbol('[').led = led

def nud(self):
    self.is_list = True
    if token.value(source) != ']':
        while True:
            if token.value(source) == ']':
                break
            self.children.append(expression())
            if token.value(source) != ',':
                break
            advance(',')
    advance(']')
    return self
symbol('[').nud = nud

def led(self, left):
    self.children.append(left)
    self.children.append(expression())
    advance('else')
    self.children.append(expression())
    return self
symbol('if').led = led

symbol(":"); symbol("=")

def parse_internal(this_node) -> ASTNode:
    global token

    def new_scope_expected():
        if token.value(source) != ':':
            raise Error('expected `:`', tokens[tokeni-1].end)
        next_token()
        assert(token.category == Token.Category.INDENT) # error message ‘expected an indented block’ is already generated by tokenizer, so there is just an assert
        next_token()

    def expected(ch):
        if token.value(source) != ch:
            raise Error('expected `'+ch+'`', token.start)
        next_token()

    def expected_name(what_name):
        next_token()
        if token.category != Token.Category.NAME:
            raise Error('expected ' + what_name, token.start)
        token_value = token.value(source)
        next_token()
        return token_value

    while token != None:
        if token.category == Token.Category.KEYWORD:
            if token.value(source) == 'def':
                node = ASTFunctionDefinition()
                node.function_name = expected_name('function name')

                if token.value(source) != '(': # )
                    raise Error('expected `(` after function name', token.start) # )(

                next_token()
                while token.value(source) != ')':
                    if token.category != Token.Category.NAME:
                        raise Error('expected function\'s argument name', token.start)
                    node.function_arguments.append(token.value(source))
                    next_token() # ((
                    if token.value(source) not in ',)':
                        raise Error('expected `,` or `)` in function\'s arguments list', token.start)
                    if token.value(source) == ',':
                        next_token()

                next_token()
                new_scope_expected()

                parse_internal(node)

            elif token.value(source) == 'class':
                node = ASTClassDefinition()
                node.class_name = expected_name('class name')

                if token.value(source) == '(':
                    node.base_class_name = expected_name('base class name')
                    expected(')')

                new_scope_expected()
                parse_internal(node)

            elif token.value(source) == 'if':
                node = ASTIf()
                next_token()
                node.expression = expression()
                new_scope_expected()
                parse_internal(node)

            elif token.value(source) == 'return':
                next_token()
                node = ASTReturn()
                if token.category in (Token.Category.DEDENT, Token.Category.STATEMENT_SEPARATOR):
                    node.expression = None
                else:
                    node.expression = expression()
                if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                    next_token()

            else:
                raise Error('unrecognized statement started with keyword', token.start)

        elif token.category == Token.Category.NAME and peek_token().value(source) == '=':
            node = ASTAssignment()
            node.dest = token
            next_token()
            next_token()
            node.expression = expression()
            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

        elif token.category == Token.Category.NAME and peek_token().value(source) == ':': # this is type hint
            var = token.value(source)
            next_token()
            type = expected_name('type name')
            type_args = []
            if token.value(source) == '[':
                next_token()
                while token.value(source) != ']':
                    type_args.append(token.value(source))
                    next_token() # [[
                    if token.value(source) not in ',]':
                        raise Error('expected `,` or `]` in type\'s arguments list', token.start)
                    if token.value(source) == ',':
                        next_token()
                next_token()

            if token != None and token.value(source) == '=':
                node = ASTAssignmentWithTypeHint()
                next_token()
                node.expression = expression()
            else:
                node = ASTTypeHint()
                if not (token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)):
                    raise Error('expected end of statement', token.start)
            node.var = var
            node.type = type
            node.type_args = type_args

            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

        elif token.category == Token.Category.DEDENT:
            next_token()
            if token.category == Token.Category.STATEMENT_SEPARATOR: # Token.Category.EOF
                next_token()
                assert(token == None)
            return this_node

        else:
            node_expression = expression()
            if token != None and token.value(source) == '=':
                node = ASTExprAssignment()
                node.dest_expression = node_expression
                next_token()
                node.expression = expression()
            else:
                node = ASTExpression()
                node.expression = node_expression
            assert(token == None or token.category in (Token.Category.STATEMENT_SEPARATOR, Token.Category.DEDENT)) # [-replace with `raise Error` with meaningful error message after first precedent of triggering this assert-]
            if token != None and token.category == Token.Category.STATEMENT_SEPARATOR:
                next_token()

        this_node.children.append(node)

    return this_node

def parse(tokens_, source_):
    global tokens, source, tokeni, token
    tokens = tokens_ + [Token(len(source_), len(source_), Token.Category.STATEMENT_SEPARATOR)]
    source = source_
    if len(tokens):
        tokeni = -1
        token = None
        next_token()
    p = ASTProgram()
    ast = parse_internal(p)

    def check_for_and_or(node):
        def f(e : SymbolNode):
            if e.symbol.id == 'or' and \
              (e.children[0].symbol.id == 'and' or e.children[1].symbol.id == 'and'):
                if e.children[0].symbol.id == 'and':
                    start = e.children[0].children[0].token.start
                    end = e.children[1].token.end
                    midend = e.children[0].children[1].token.end
                    midstart = e.children[0].children[1].token.start
                else:
                    start = e.children[0].token.start
                    end = e.children[1].children[1].token.end
                    midend = e.children[1].children[0].token.end
                    midstart = e.children[1].children[0].token.start
                raise Error('relative precedence of operators `and` and `or` is undetermined; please add parentheses this way: `(' \
                    + source[start:midend  ] + ')' + source[midend  :end] + '` or this way: `' \
                    + source[start:midstart] + '(' + source[midstart:end] + ')`', start)

        node.walk_expressions(f)

        if isinstance(node, ASTNodeWithChildren):
            for child in node.children:
                check_for_and_or(child)
    check_for_and_or(ast)

    return ast
