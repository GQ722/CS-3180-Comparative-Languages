# Name: Devesh Patel (Former name: Devesh Amin)
# Class: CS 3180
# Date: March 15, 2016
# Description: This file contains functions that implements a scanner for numbers, symbols, comments, arithmetic operators, parenthesis, and EOF

import ply.lex as lex

# List to scanner tokens names
tokens = ( 'NUM', 'ADD', 'SUBSTRACT', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN', 'EOF' )

# Regular expression for the above token names
t_ADD = r'\+'
t_SUBSTRACT = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_ignore = ' \t'

#
def t_NUM(t):
    r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?'
    t.value = int(t.value)
    return t

# Rule for new line
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

# Error handling
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# EOF handling rule
# def t_eof(t):
#    more = raw_input('... ')
#    if more:
#        self.lexer.input(more)
#        return self.lexer.token()
#    return None

# Comment handling
# def t_comment(t):
#    r'\#.*'
#    pass              #We can ignore comments with t_ignore = ' //'

# Build lexer
lexer = lex.lex()

# Test case
test ='''
((3 + 4) / 10)
  + -20 *2
'''

# Passing the test case to lexer / scanner
lexer.input(test)

# Tokenize the output
while True:
    tkn = lexer.token()
    if not tkn:
        break      # No more input
    print(tkn.type, tkn.value, tkn.lineno, tkn.lexpos)


