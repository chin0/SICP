#lang sicp
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? x) (= 0 (remainder x 2)))
(define (fast-mul a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-mul a (+ b b) (halve n)))
        (else (fast-mul (+ a b) b (- n 1)))))