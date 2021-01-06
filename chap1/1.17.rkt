#lang sicp
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? x) (= 0 (remainder x 2)))
(define (fast-mul a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))