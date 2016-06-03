;; Name: Devesh Patel (Former name: Devesh Amin)
;; Class: CS 3180
;; Date: February 26, 2016
;; Description: This file contains functions that implements a scanner for numbers, symbols, comments, arithmetic operators, parenthesis, and EOF

#lang racket

;; Racket supplied lexicographic scanning using regular expressions
(require parser-tools/yacc
         parser-tools/lex (prefix-in : parser-tools/lex-sre) syntax/readerr)

;; Define the tokens our parser will expect from the scanner
(define-tokens value-tokens (NUM ERROR))
(define-empty-tokens punct-tokens (ADD SUBSTRACT MULTIPLY DIVIDE LPAREN RPAREN EOF))

;; Define some patterns to save typing later
(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev letter (:or (:/ "A" "Z") (:/ "a" "z")))

;; Comments are "//" followed by any characters that aren't #\newline
(define-lex-abbrev comment-re (:: "//"
   (complement (:: any-string #\newline any-string)) #\newline))

;; The scanner/lexer is now defined below using lexer-src-pos
;; The lexer-src-pos is a function which scans an input port and returns next position in the input stream; one at a time.
(define get-token
  (lexer-src-pos

    ;; Take whitespace as input and skip every position of it recursively 
    [(:or #\tab #\space #\newline) (return-without-pos (get-token input-port))]

    ;; The literal string "//" followed by zero or more characters until the end of the line
    [comment-re (return-without-pos (get-token input-port))]
    
    ;; One or more digits
    [(:+ digit) (token-NUM (string->number lexeme))]
    
    ;; Zero of more digits followed by a decimal point followed by one or more digits
    [(:: (:* digit) #\. (:+ digit)) (token-NUM (string->number lexeme))]

    ;; One or more digits followed by a decimal point followed by zero or more digits
    [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]

    ;; Addition arithmetic operator
    [(:: #\+) 'ADD ]

    ;; Substraction arithmetic operator
    [(:: #\-) 'SUBSTRACT ]
    
    ;; Multiplication arithmetic operator
    [(:: #\*) 'MULTIPLY]

    ;; Division arithmetic operator
    [(:: #\/) 'DIVIDE ]

    ;; Left parenthesis 
    [(:: #\() 'LPAREN ]

    ;; Right parenthesis
    [(:: #\)) 'RPAREN ]

    ;; The literal end of file input as EOF
    [(eof)
     'EOF]
    
    ;; The last rule returns an ERROR token for any characters not otherwise handled
    (any-char (token-ERROR lexeme))
  )
)

;; Returns a list of all tokens found read from in-port
(define (build-token-list a-pos-token in-port)
  (cond
    [(eq? 'EOF (token-name (position-token-token a-pos-token)))
       (list a-pos-token)
    ]
    [else (cons a-pos-token (build-token-list (get-token in-port) in-port))]
  )
)

;; Displays all tokens in a a-token-list
(define (display-it a-token-list)
  (cond
     [(empty? a-token-list) (void)]
     [else
        (begin
          (display (token-name (position-token-token (car a-token-list))))
          (display " = ")
          (displayln (token-value (position-token-token (car a-token-list))))
          (display-it (cdr a-token-list))
        )
      ]
  )
)

;; Configure input-port to count lines, build a list of all tokens read from and input-port, and then parse the list of tokens indicate of whether the tokens are accepted by the BNF grammar
(define (start-it input-port)
  (begin
    (port-count-lines! input-port)
    (display-it (build-token-list (get-token input-port) input-port))
  )
)

;; Read test cases from a file for scanning
(start-it (open-input-file "SmallProject02.txt"))