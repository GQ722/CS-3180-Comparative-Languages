###################################################################################
## Name: Devesh Patel
## Class: CS 3180
## Project: Project 2
## Date: April 12, 2016
###################################################################################

import sys

sys.path.insert(0, "../..")

if sys.version_info[0] >= 3:
    raw_input = input

######################################################################
######################################################################
# Scanner generation
tokens = ('NUM', 'NAME', 'WHILE','PLUSPLUS','MINUSMINUS','PRINT' ,'TERN')

literals = ['+', '*', '(', ')', '=', '{', '}', ';','/','-','>',':','?','[',']']
t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_WHILE = r'@While'
t_PRINT = r'@Print'
t_TERN = r'@Tern'
t_PLUSPLUS = r'\+\+'
t_MINUSMINUS = r'--'

def t_NUM(t):
    r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?'
    t.value = float(t.value)
    return t

t_ignore = " \t\r"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
import ply.lex as lex

lex.lex()


################################################################################
## Sample BNF Grammar for expressions. Note: this grammar avoids left recursion
## making it easier to support LL recursive descent parsing.
## <statement> ::= <expr> ";"
##             | <block>
##             | WHILE "(" <expr> ")" <statement>
##             | PRINT "(" <statement> ")"
##             | TERN "[" <expr> "?" <statement> ":" <statement> "]"
##
## <block> ::= "{" <block_item_list> "}"
##         | "{" "}"
##
## <block_item_list> ::= <statement>
##                   | <block_item_list> <statement>
##
## <expr> ::- <term> ADD <expr>
##               | <term> MINUS <expr>
##               | <term>
##               | <comparator>
##               |NAME PLUSPLUS
##               |NAME MINUSMINUS
##               | NAME = <expr>
##               | "(" expr ")" '?' expr ':' expr
##
## <term> ::- <factor> MULTIPLY <term>
##                |<factor> DIVIDE <term>
##                | <factor>
##
## <comparator> ::= <term> GREATER <expr>
##
## <factor> ::- LPAREN <expr> RPAREN
##                 | NUM
##                 | NAME
################################################################################

#################################################################################
#helper functions
def set_variable(node):
    Node.varsDict[node.children[0]] = node.children[1].interp()
    return Node.varsDict[node.children[0]]

def get_variable(node):
   try:
      return Node.varsDict[node.text]
   except LookupError:
      print("Undefined name '%s'" % node.text)
      return 0

# NAME = NAME + 1
def increment_var(node):
    Node.varsDict[node.children[0]] += 1
    return Node.varsDict[node.children[0]]


# NAME = NAME - 1
def decrement_var(node):
    Node.varsDict[node.children[0]] -= 1
    return Node.varsDict[node.children[0]]

def while_helper(node):
    result = 0
    while node.children[0].interp():
        result = node.children[1].interp()
    return result

def print_helper(node):
    result = node.children[0].interp()
    print('"' + str(result) + '"')
    return result

def block_interp_helper(node):
    Node.localStack.append(node.localVars)
    result = 0
    for child in node.children :
        result = child.interp()
    Node.localStack.pop()
    return result

def ternary_helper(node):
    if node.children[0].interp():
        return node.children[1].interp()
    else:
        return node.children[2].interp()

##################################################################################

# A Node class to represent each node in a parse tree
class Node:
    varsDict = {}
    localStack = []

    def __init__(self):
        """ Initializes self to an invalid state """
        self.children = []
        self.text = "invalid"
        self.function = lambda node: node.text

    def interp(self):
        """ Interprets the parse tree rooted at self """
        return self.function(self)

class BlockNode(Node):
   def __init__(self):
      Node.__init__(self)
      self.localVars = {}

def p_statement_expr(p):
    ' statement : expr ";" '
    p[0] = p[1]

def p_statement_block(p):
    ' statement : block '
    p[0] = p[1]

def p_statement_while(p):
    ' statement : WHILE "(" expr ")" statement '
    p[0] = Node()
    p[0].text = "WHILE"
    p[0].children = [p[3], p[5]]
    p[0].function = while_helper

def p_statement_print(p):
    'statement : PRINT "(" statement ")" '
    p[0] = Node()
    p[0].text = "PRINT"
    p[0].children = [p[3]]
    p[0].function = print_helper

def p_statement_tern(p):
    'statement : "[" TERN expr "?" statement ":" statement "]" '
    p[0] = Node()
    p[0].text = "ternary"
    p[0].children = [p[3], p[5], p[7]]
    p[0].function = lambda node: ternary_helper(node)



def p_block_empty(p):
    ' block : "{" "}" '
    p[0] = BlockNode()
    p[0].text = "block"
    p[0].children = []
    p[0].function = lambda node: 0

def p_block_with_list(p):
    ' block : "{" block_item_list "}" '
    p[0] = BlockNode()
    p[0].text = "block"
    p[0].children = p[2]
    p[0].function = block_interp_helper

def p_block_item_list_statement(p):
    ' block_item_list : statement '
    p[0] = [ p[1] ]

def p_block_item_list_compound(p):
    ' block_item_list : block_item_list statement '
    p[0] = p[1]
    p[0].append(p[2])



def p_expr_a(p):
    ' expr : term "+" expr'
    p[0] = Node()
    p[0].text = "+"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() + node.children[1].interp()

def p_expr_b(p):
    ' expr : term'
    p[0] = p[1]

def p_expr_nm(p):
    'expr : NAME "=" expr'
    p[0] = Node()
    p[0].text = '='
    p[0].children = [p[1],p[3]]
    p[0].function = lambda node: set_variable(node)

def p_expr_sub(p):
    """expr : term "-" expr"""
    p[0] = Node()  # Node instance
    p[0].text = "-"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() - node.children[1].interp()

def p_expr_comp(p):
    """expr : comparator"""
    p[0] = p[1]

def p_expr_inc(p):
    """ expr : NAME PLUSPLUS """
    p[0] = Node()
    p[0].text = '++'
    p[0].children = [p[1]]
    p[0].function = lambda node: increment_var(node)

def p_expr_dec(p):
    """ expr : NAME MINUSMINUS """
    p[0] = Node()
    p[0].text = '--'
    p[0].children = [p[1]]
    p[0].function = lambda node: decrement_var(node)


def p_term_m(p):
    'term : factor "*" term '
    p[0] = Node()
    p[0].text = "*"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() * node.children[1].interp()

def p_term(p):
    '''term : factor'''
    p[0] = p[1]  # Return a Node instance (p[1] is already a Node)

def p_term_b(p):
    """term : factor '/' term"""
    p[0] = Node()  # Return a Node instance
    p[0].text = "/"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() / node.children[1].interp()

def p_comparator_a(p):
    """comparator : factor '>' factor"""
    p[0] = Node()  # Return a Node instance
    p[0].text = '>'
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() > node.children[1].interp()

def p_factor_a(p):
    '''factor : "(" expr ")"'''
    p[0] = p[2]

def p_factor_b(p):
    '''factor : NUM'''
    p[0] = Node()
    p[0].text = str(p[1])
    tmp = p[1]
    p[0].function = lambda node: tmp

def p_factor_nm(p):
   '''factor : NAME'''
   p[0] = Node()
   p[0].text = p[1]
   p[0].function = lambda node: get_variable(node)

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

# Prints the text of each node in parse tree
def interpret_tree(rootNode):
    if not rootNode.children:
        print(rootNode.text)
    else:
        for node in rootNode.children:
            interpret_tree(node)
        print(rootNode.text)


import ply.yacc as yacc

yacc.yacc()

############################################################################################
# proj2_test.txt file demonstrates all requirements for Project 2
# if proj2_test.tx doesn't exist then gets user input from console and user can manual enter
# and check all the Project 2 requirements
############################################################################################
try:
    f = open('proj2_test.txt', 'r')
    for line in f:

        if not line: continue
        rootNode = yacc.parse(line + '\n')

        if None != rootNode:
            print(rootNode.interp())

    f.close()

except IOError:
       while 1:
            try:
                s = raw_input("input > ")
            except EOFError:
                break
            if not s: continue
            rootNode = yacc.parse(s + '\n')  # parse() returns None upon error
            #    if None != rootNode:
            #        node_printer(rootNode)   # print result of interpreting the entire node tree
            if None != rootNode:
                print(rootNode.interp())


