###################################################################################
## Name: Devesh Patel
## Class: CS 3180
## Project: Small Project 03
## Purpose: To build a BNF parser tree that parses the input in RPN (postfix) form.
## Date: April 1, 2016
###################################################################################

## No imports needed

###################################################################################
## Tokens for the paser
###################################################################################
ADD = "+"
SUB = "-"
MUL = "*"
DIV = "/"
LPAREN = "("
RPAREN = ")"
EOF = "EOF"


####################################################################################
## Class that defines reverse polish notation parser.
## Simple way to build a parser without using raw_input.
## No lex or yacc used
####################################################################################
class RPNParser:
    """Parses a token list and returns a nested exp structure in RPN form."""

    def __init__(itself, tokens):
        itself.tokens = tokens

    def peek_token(itself):
        """Peek: just look at the next token in the list"""
        return itself.tokens[0]

    def pop_token(itself):
        """Pop: pop the next token in the list"""
        next_token = itself.tokens[0]
        del itself.tokens[0]
        return next_token

    ####################################################################################
    ## Simple parser for Exp, Factor and Term that comply with RPN form.
    ####################################################################################
    def exp_parse(itself):
        """Exp: consist of factors, returns parsed exp"""
        exp = itself.factor_parse()
        while True:
            if itself.peek_token() in (ADD, SUB):
                root = itself.pop_token()
                factor = itself.factor_parse()
                exp = [exp, factor, root]
            else:
                break
        return exp

    def factor_parse(itself):
        """Factor: consist of terms, returs a parsed factor"""
        factor = itself.term_parse()
        while True:
            if itself.peek_token() in (MUL, DIV):
                root = itself.pop_token()
                term = itself.term_parse()
                factor = [factor, term, root]
            else:
                break
        return factor

    def term_parse(itself):
        """Term: consist of number or left and right parantheses, returns parsed term"""
        if itself.peek_token() != LPAREN:
            return int(itself.pop_token())
        else:
            itself.pop_token()  ## get rid of LPAREN
            exp = itself.exp_parse()
            itself.pop_token()  ## get rid of RPAREN
            return exp


########################################################################################3
## Test cases for RPN parser
## Test case 1: input: (5 + 3) * 8 -> output: 5 3 + 8 *
## Test case 2: input: (5 + 3 * 8) -> output: 5 3 8 * +
## Test case 3: input: (1 * 5 + 3) - 1 + (2 * 0) / (3 / 2) + 9
##                                           - > output:1 5 * 3 + 1 - 2 0 * 3 2 / / + 9 +
#########################################################################################
def main():
    print("Test case 1: Input: (5 + 3) * 8")
    print("Output:", RPNParser([LPAREN, 5, ADD, 3, RPAREN, MUL, 8, EOF]).exp_parse())
    print("\n")
    print("Test case 2: Input: (5 + 3 * 8)")
    print("Output:", RPNParser([LPAREN, 5, ADD, 3, MUL, 8, RPAREN, EOF]).exp_parse())
    print("\n")
    print("Test case 3: Input: (1 * 5 + 3) - 1 + (2 * 0) / (3 / 2) + 9")
    print("Output:", RPNParser(
        [LPAREN, 1, MUL, 5, ADD, 3, RPAREN, SUB, 1, ADD,
         LPAREN, 2, MUL, 0, RPAREN, DIV, LPAREN, 3, DIV,
         2, RPAREN, ADD, 9, EOF]).exp_parse())

if __name__ == '__main__':
    main()
