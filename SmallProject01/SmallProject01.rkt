;;Name: Devesh Amin
;;Class: CS 3180
;;Date: February 6, 2016
;;Description: This file contains function that reads the file and displays the output as specified.


#lang racket

(require 2htdp/batch-io)

;; function that generates all the words in the lowercase from the file linuxwords.txt
(define all-words (map string-downcase(read-lines "linuxwords.txt")))

;; function that only generates the words that has lenght of 6 characters
(define all-six-letter-words (filter (lambda (str)
                                       (
                                        eq? 6 (string-length str)
                                       ))
                                     all-words))

;; function that generates all the words that do not contain a e i o
(define (no-aeio str)
  [cond
    [(member #\a (string->list str)) #f]
    [(member #\e (string->list str)) #f]
    [(member #\i (string->list str)) #f]
    [(member #\o (string->list str)) #f]
    [else #t]
  ]
)

(display "The following are all the six letter words that do not conatain a e i o in the file linuxwords.txt: \n\n") ;; display the given statement
(display (string-join (filter no-aeio all-six-letter-words) ", "))   ;; dispaly all the words with comma ',' seperated using the no-aeio algorithm


;; My first approach - didn't work for lowercase words and din't print with comma seperated words.
;; Read the file and print all the words that do not contain a e i o

;;(call-with-input-file* "linuxwords.txt"                     
;; (lambda (words)
;;         (for/list ([i (in-lines words)]
;;                    #:when (and (= (string-length i) 6)
;;                                (not (string-contains? i "a"))
;;                                (and (not (string-contains? i "e" )))
;;                                (and (not (string-contains? i "i")))
;;                                (and (not (string-contains? i "o")))
;;                                (and (not (string-contains? i "o")))
;;                                (and (not (string-contains? i "A")))
;;                                (and (not (string-contains? i "E")))
;;                                (and (not (string-contains? i "I")))
;;                                (and (not (string-contains? i "O")))
;;                                )
;;                    )
;;         i)
;;    )
;;  )

;;number of u's in the linuxwords.txt
(define (count-u u-lst)
  (cond
    [(null? u-lst) 0]
    [(eq? (car u-lst) #\u) (+ 1 (count-u (cdr u-lst)))]
    [else (count-u (cdr u-lst))]
  )
)

(display "\n\nNumber of times the letter 'u' appears in the linuxwords.txt file is: ")  ;; Dispaly the given statement
(print (foldl + 0 (map count-u (map string->list all-words))))                    ;; run the count-u algorithm in the list form in the file to count the number of u's