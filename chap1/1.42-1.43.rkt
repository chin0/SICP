#lang sicp
(define (compose f g) (lambda (x) (f (g x))))
(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))