;;Name: Devesh Amin, Collaborator: Erik Buck
;;Class: CS 3180
;;Date: February 10, 2016
;;Description: 

#lang racket

(require 2htdp/batch-io)

;; Define 'store' as a list of words found in the 'linuxwords.txt' in downcase
(define store (map string-downcase(read-lines "linuxwords.txt")))

;; Define 'four-or-longer' as a list of all four or longer words in 'store'
(define four-or-longer (filter (lambda (str)
                                       (
                                        < 3 (string-length str)
                                       ))
                                     store))

;;defining the random letters
;;(define letters
;;(string->list "abcdefghijklmnopqrstuvwxyz"))

;;defining the matrix
(define (make-matrix list-of-strings)
  (map string->list list-of-strings)
)

;;char at position x,y in the matrix
(define (char-at x y matrix)
  (list-ref (list-ref matrix y) x) ;; list-ref O(n)
)

;; test
(define test(list four-or-longer))

;; Inserts all items in row at sequential x positions of row y starting with x 
(define (hash-matrix-insert-row hash-matrix row x y)
  [cond
     [(null? row)]
     [else
         (hash-set! hash-matrix (list x y) (car row))
         (hash-matrix-insert-row hash-matrix (cdr row) (+ 1 x) y)
         hash-matrix
     ]
  ]
)

;; Inserts information from all rows in matrix into hash-matrix
(define (hash-matrix-insert-rows hash-matrix matrix y)
  [cond
     [(null? matrix) hash-matrix]
     [else
         (hash-matrix-insert-row hash-matrix (car matrix) 0 y)
         (hash-matrix-insert-rows hash-matrix (cdr matrix) (+ 1 y))
         hash-matrix
     ]
  ]
)

;; Returns a hash-matrix containing the same information in matrix. The
;; hash-matrix provides O(1) lookup for values
;; Map position in matrix -> char
(define (make-hash-matrix matrix) (hash-matrix-insert-rows (make-hash) matrix 0))

;; Returns the character at x,y in hash-matrix or #\? if no character exists
;; at that position
(define (hash-matrix-char-at hash-matrix x y) (hash-ref hash-matrix (list x y) #\?)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section provides functions for creating and using a hash-letters mapping
;; letter to a list of positions in the matrix where each position is a list
;; containing x,y coordinates.
;; Use hash-letters to find all the positions within a matrix where each letter
;; can be found.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Appends a list containing x,y position of each letter in row to the list
;; of positions for each character
(define (hash-letters-insert-row letters-matrix row x y)
  [cond
     [(null? row)]
     [else
         (hash-set! letters-matrix (car row) (cons (list x y)
                                     (hash-ref letters-matrix (car row) '())))
         (hash-letters-insert-row letters-matrix (cdr row) (+ 1 x) y)
         letters-matrix
     ]
  ]
)

;; Inserts information from all rows in matrix into letters-matrix
(define (hash-letters-insert-rows letters-matrix matrix y)
  [cond
     [(null? matrix) letters-matrix]
     [else
         (hash-letters-insert-row letters-matrix (car matrix) 0 y)
         (hash-letters-insert-rows letters-matrix (cdr matrix) (+ 1 y))
         letters-matrix
     ]
  ]
)

;; Returns a hash table containing a list of positions for each letter in
;; matrix. The hash table provides O(1) lookup for values
;; Map char -> list of position in matrix
(define (make-hash-letters matrix) (hash-letters-insert-rows (make-hash) matrix 0)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section provides functions for finding words within a hash-matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #t if and only if all letters in word can be found reading left to
;; right starting at position x,y in hash-matrix
(define (is-word-at-left word hash-matrix x y)
   [cond
      [(null? word) #t] ; found all letters in word
      [else 
         (and
            (eq? (car word) (hash-matrix-char-at hash-matrix x y))
            (is-word-at-left (cdr word) hash-matrix (+ 1 x) y)
         )
      ]
   ]
)


;; Returns #t if and only if all letters in word can be found reading top to
;; bottom starting at position x,y in hash-matrix
(define (is-word-at-down word hash-matrix x y)
   [cond
      [(null? word) #t] ; found all letters in word
      [else 
         (and
            (eq? (car word) (hash-matrix-char-at hash-matrix x y))
            (is-word-at-down (cdr word) hash-matrix x (+ 1 y))
         )
      ]
   ]
)


;; Returns #t if and only if all letters in word can be found reading diagonally
;; top left to bottom right starting at position x,y in hash-matrix
(define (is-word-at-leftdown word hash-matrix x y)
   [cond
      [(null? word) #t] ; found all letters in word
      [else 
         (and
            (eq? (car word) (hash-matrix-char-at hash-matrix x y))
            (is-word-at-leftdown (cdr word) hash-matrix (+ 1 x) (+ 1 y))
         )
      ]
   ]
)


;; Returns #t if and only if all letters in word can be found reading diagonally
;; bottom left to top right starting at position x,y in hash-matrix
(define (is-word-at-leftup word hash-matrix x y)
   [cond
      [(null? word) #t] ; found all letters in word
      [else 
         (and
            (eq? (car word) (hash-matrix-char-at hash-matrix x y))
            (is-word-at-leftup (cdr word) hash-matrix (+ 1 x) (- y 1))
         )
      ]
   ]
)


(define (is-word-in-hash-matrix word hash-matrix letter-matrix)
  (or
     ;; Look left to right
     (ormap
         (lambda (pos) (is-word-at-left word hash-matrix (list-ref pos 0)
                                        (list-ref pos 1)))
         (hash-ref letter-matrix (car word) '())
     )
     ;; Look top to bottom
     (ormap
         (lambda (pos) (is-word-at-down word hash-matrix (list-ref pos 0)
                                        (list-ref pos 1)))
         (hash-ref letter-matrix (car word) '())
     )
     ;; Look diagonal top left to bottom right
     (ormap
         (lambda (pos) (is-word-at-leftdown word hash-matrix (list-ref pos 0)
                                        (list-ref pos 1)))
         (hash-ref letter-matrix (car word) '())
     )
     ;; Look diagonal bottom left to top right
     (ormap
         (lambda (pos) (is-word-at-leftup word hash-matrix (list-ref pos 0)
                                        (list-ref pos 1)))
         (hash-ref letter-matrix (car word) '())
     )
  )
)

(define (find-words-helper2 hash-matrix hash-letters)
   (filter
      (lambda (word)
         (or
            (is-word-in-hash-matrix (string->list word)
               hash-matrix hash-letters)        
            (is-word-in-hash-matrix (reverse (string->list word))
               hash-matrix hash-letters
            )
         )
      )
      four-or-longer
   )
)


(define (find-words-helper matrix)
   (find-words-helper2 (make-hash-matrix matrix) (make-hash-letters matrix))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CONTRACT: find-words : width length letters -> List
; PURPOSE: Returns a list of dictionary words 4 letters or longer
; that can be found in a matrix of letters that has dimensions width
; by length. Widths and length are natural numbers.
; CODE:
(define (find-words width length letters)
   (find-words-helper (make-matrix letters))
)

;; Define some test input
(define test-input '(
                     "zurichzoom"
                     "dogstdogst"
                     "flogoflogo"
                     "zlzppzlzpp"
                     "flingfling"
                     "Louisianan" 
                    )
)

;; Test make-matrix
;(char-at 1 1 (make-matrix test-input))

;; Test make-hash-matrix
;(displayln (make-hash-matrix (make-matrix test-input)))
;(println (hash-matrix-char-at (make-hash-matrix (make-matrix test-input)) 1 1))

;; Test find-words
(displayln (find-words 6 10 test-input))