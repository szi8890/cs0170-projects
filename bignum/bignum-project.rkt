;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bignum-project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "bignum-operators.rkt")

;; input: a list L with all elements as numbers from 0-9
;; output: the number 0 if the list is empty or the first element of list L if it is not empty

(define first-empty
  (lambda (L)
    (if (empty? L) 0 (first L))))

;; test-cases:
(check-expect (first-empty empty) 0)
(check-expect (first-empty (quote (1))) 1)
(check-expect (first-empty (quote (2 3))) 2)
(check-expect (first-empty (quote (2 3 4))) 2)

;; input: a list M with all elements as numbers from 0-9
;; output: the empty list if the list is empty or the rest of the list L if not empty

(define rest-empty
  (lambda (M)
    (if (empty? M) empty (rest M))))

;; test-cases
(check-expect (rest-empty empty) empty)
(check-expect (rest-empty (quote (1))) empty)
(check-expect (rest-empty (quote (2 3))) (quote (3)))
(check-expect (rest-empty (quote (3 4 5))) (quote (4 5)))


;; input: a list A with all elements as numbers from 0-9
;; output: adding 1 to the first element in the list and adding it back into the
;;         list at the same index

(define carry-over
  (lambda (A)
    (cons (digit-add 1 (first-empty (rest-empty A))) (rest-empty (rest-empty A)))))

;; test-cases:
(check-expect (carry-over empty) (quote (1)))
(check-expect (carry-over (quote (1))) (quote (1)))
(check-expect (carry-over (quote (9))) (quote (1)))

;; carry-list is an empty list that serves to help facilitate adding zeroes throughout bignum

(define carry-list empty)

;; input: two lists of numbers bignum1 and bignum 2
;; output: a single list which adds bignum1 and bignum2 together, accounting for scenarios
;;         in which a 1 needs to be carried over.

;; recursion diagrams:
;; input: (1 2) (2 3)
;;   recursive input: (2) (3)
;;   recursive output: (5)
;; output: (3 5)

;; input: (1 2 3 4 5) (1 2)
;;   recursive input: (2 3 4 5) (2)
;;   recursive output: (4 3 4 5)
;; output: (2 4 3 4 5)

;; input: (1) ()
;;   recursive input: none
;;   recursive output: none
;; output: (1)

(define bignum+
  (lambda (bignum1 bignum2)
    (cond
      [(and (empty? bignum1) (empty? bignum2)) empty]
      [true (if (> (digit-add (first-empty bignum1) (first-empty bignum2)) 9)
                (cons (digit-rem (digit-add (first-empty bignum1) (first-empty bignum2)) 10)
                      (bignum+ (carry-over bignum1) (rest-empty bignum2)))
                (cons (digit-add (first-empty bignum1) (first-empty bignum2))
                      (bignum+ (rest-empty bignum1) (rest-empty bignum2))))])))

;; test-cases:
(check-expect (bignum+ empty empty) empty)
(check-expect (bignum+ (quote (1)) empty) (quote (1)))
(check-expect (bignum+ empty (quote (2))) (quote (2)))
(check-expect (bignum+ (quote (1 2)) (quote (2 3))) (quote (3 5)))
(check-expect (bignum+ (quote (9 9)) (quote (2 3))) (quote (1 3 1)))
(check-expect (bignum+ (quote (1 2 3 4 5)) (quote (1 2))) (quote (2 4 3 4 5)))
(check-expect (bignum+ (quote (9 1)) (quote (4 5 6 7 8))) (quote (3 7 6 7 8)))
(check-expect (bignum+ (quote (9 9 9 9 9 9 9 9 9 9 9 9)) (quote (9 9 9 9 9 9 9 9 9)))
                       (quote (8 9 9 9 9 9 9 9 9 0 0 0 1)))


;; intput: a list of numbers num1 and num2. num1 is list with one element whereas num2 is a
;;         list with as many elements
;; output: a list where each element of num2 is multiplied against num1 (the first element)

;; recursion diagrams: 
;; input: (5) (1 2 3)
;;   recursive input: (5) (2 3)
;;   recursive output: (0 6 1)
;; output: (5 0 6 1)

;; input: (9) (1 1 1)
;;   recursive input: (9) (1 1)
;;   recursive output: (9 9)
;; output: (9 9 9)

;; input: (1) ()
;;   recursive input: none
;;   recursive output: none
;; output: ()

(define lil-mult
  (lambda (num1 num2)
    (cond
      [(or (empty? num1) (empty? num2)) empty]
      [true (if (> (digit-mult (first-empty num1) (first-empty num2)) 9)
                (bignum+ (cons (digit-rem (digit-mult (first-empty num1) (first-empty num2)) 10)
                               (lil-mult num1 (rest-empty num2)))
                         (cons 0 (cons (digit-quo (digit-mult (first-empty num1)
                                 (first-empty num2)) 10) carry-list)))
                (bignum+ (cons (digit-mult (first-empty num1) (first-empty num2))
                               (lil-mult num1 (rest-empty num2)))
                         (cons 0 carry-list)))])))

;; test-cases:
(check-expect (lil-mult empty empty) empty)
(check-expect (lil-mult (quote (1)) empty) empty)
(check-expect (lil-mult empty (quote (1 2))) empty)
(check-expect (lil-mult (quote (9)) (quote (1 1 1))) (quote (9 9 9)))
(check-expect (lil-mult (quote (5)) (quote (3 2 3))) (quote (5 1 6 1)))
(check-expect (lil-mult (quote (1)) (quote (5 4 5))) (quote (5 4 5)))

;; input: two lists of numbers bignum3 and bignum4
;; output: the list that is the long multiplication of each other

;; recursion diagrams:
;; input: (0 1 2 3) (2 2)
;;   recursive input: (1 2 3) (2 2)
;;   recursive output: (2 6 0 7)
;; output: (0 2 6 0 7)

;; input: (1 2 3 4) (1)
;;   recursive input: (2 3 4) (1)
;;   recursive output: (2 3 4)
;; output: (1 2 3 4)

;; input: (1 2 3) (5 5 5 5)
;;   recursive input: (1 2 3) (5 5 5)
;;   recursive output: (5 6 2 8 6)
;; output: (5 5 1 3 8 7 1)

;; input: (2 3 1) ()
;;   recursive input: none
;;   recursive output: none
;; output: ()

(define bignum*
  (lambda (bignum3 bignum4)
    (cond
      [(empty? bignum4) empty]
      [(empty? (rest-empty bignum3)) (lil-mult bignum3 bignum4)]
      [true (bignum+ (lil-mult bignum3 bignum4) (cons 0 (bignum* (rest-empty bignum3) bignum4)))])))

;; test-cases
(check-expect (bignum* empty empty) empty)
(check-expect (bignum* (quote (2 3 1)) empty) empty) 
(check-expect (bignum* empty (quote (1 2))) empty)
(check-expect (bignum* (quote (1)) (quote (5))) (quote (5)))
(check-expect (bignum* (quote (1 2 3 4)) (quote (1))) (quote (1 2 3 4)))
(check-expect (bignum* (quote (0 1 2 3)) (quote (2 2))) (quote (0 2 6 0 7)))
(check-expect (bignum* (quote (1 2 3)) (quote (5 5 5 5))) (quote (5 5 1 3 8 7 1)))
(check-expect (bignum* (quote (1 4 3 2 1)) (quote (1 3 4 4 9)))
                       (quote (1 7 9 2 7 3 5 6 1 1)))