;;Name: Devesh Amin
;;Small Project 00

#lang racket

;;Question 1 - function that takes one or more numeric arguments and returns the largest of them
(define (max2 a b)  ;;takes two numbers as arguments and returns whichever number is larger
  (if (> b a) b a))

(define (max3 num1 . num2) ;;takes one or more number and returns whicheve number is larger
  (if (empty? num2) num1
    (apply max3 (cons (max2 num1 (car num2))  (cdr num2)))))

;;Question 2 - function that returns #t if the second argument appears in
;;             the list argument before the third argument otherwise returns #f
(define (before-in-list? this first second)
  (cond ((empty? this) #f)
        ((eq? (car this) first) #t)
        ((eq? (cdr this) second) #t)
        ((eq? (car this) second) #f)
        (else (before-in-list? (cdr this) first second))))

;;Question - 3
(define (even symbols)  ;;Even function returns the elements of the list in even position
  (if (or (empty? symbols) (empty? (cdr symbols))) '()                         
      (cons (cadr symbols) (even (cddr symbols)))))

(define (odd symbols)   ;;Odd function returns the elements of the list in odd position
  (if (or (empty? symbols) (empty? (cdr symbols))) '()
      (cons (car symbols) (odd (cddr symbols)))))

;;Question - 4 function duplicates each char / numbers
(define (duplicate1 lst)
  [cond
    [(empty? lst) '()]
    [(list? (car lst)) (cons (duplicate1 (car lst)) (duplicate1 (rest lst)))]
    [else (cons (car lst) (cons (car lst) (duplicate1 (rest lst))))]])

;;Question 5 - function calculates the dotProduct of two vectors
(define (dotProduct lst1 lst2)
  (cond [(empty? lst1) 0]
        [(> (length lst1) (length lst2)) (println "*Incompatible*, Try again with appropriate length of each vector")]
        [(+ (* (car lst1) (car lst2)) (dotProduct (cdr lst1) (cdr lst2)))]))

;;Question 6 - lastLess
(define (lastLess lst rmv)
  (reverse (remove rmv (reverse lst))))

;;Question 7
;; (define (typer this-nested-list)
;;   [cond
;;      [(empty? this-nested-list) '()]
;;      [(if list contains number?, swap all the number to n)
;;       (if list contains symbol?, swap all the symbols to s)]       
