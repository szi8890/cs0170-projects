#lang racket/base
(provide digit-add
         digit-sub
         digit-mult
         digit-quo
         digit-rem)

;; This file provides the built-in arithmetic procedures required
;; to implement limited precision arithmetic with bignums.


;; Input: two natural numbers between 0 and 99, a and b
;; Output: the sum of the a and b, or an error if
;;         a, b or their sum is outside the range [0,99]

(define digit-add 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99)
            (<= (+ a b) 99))
        (+ a b)
        (error 'add "digit-add inputs or output out of range. tried (+ ~a ~a) " a b))))



;; Input: two natural numbers between 0 and 99, a and b
;; Output: the result of subtracting b from a, or an error if
;;         a or b or their difference is outside the range [0,99]

(define digit-sub 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99))
      (- a b)
      (error 'add "digit-sub inputs or output out of range. tried (- ~a ~a) " a b))))


;; Input: two natural numbers between 0 and 99, a and b
;; Output: the product of the inputs, or an error if
;;         a or b or their product is outside the range [0,99]

(define digit-mult 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99)
            (<= (* a b) 99))
        (* a b)
        (error 'add "digit-mult inputs or output out of range. tried (* ~a ~a) " a b))))


;; Input: two natural numbers between 0 and 99, a and b
;; Output: the quotient of dividend a and divisor b, or an error if
;;         a or b or their quotient is outside the range [0,99]

(define digit-quo 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99))
        (quotient a b)
        (error 'add "digit-quo inputs or output out of range. tried (quotient ~a ~a) " a b))))


;; Input: two natural numbers between 0 and 99, a and b
;; Output: the remainder of dividend a and divisor b, or an error if
;;         a or b or the remainder is outside the range [0,99]

(define digit-rem 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99))
        (remainder a b)
        (error 'add "digit-rem inputs or output out of range. tried (remainder ~a ~a) " a b))))

