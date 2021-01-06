#lang sicp
(define (pascal n r)
  (cond ((= n r) 1)
        ((= r 1) 1)
        (else (+ (pascal (- n 1) r) (pascal (- n 1) (- r 1))))))