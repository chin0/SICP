#lang sicp
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0) result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (square x) (* x x))
(define (tan-cf x k)
  (cont-frac (lambda (k) (if (<= k 1) x (- (square x))))
             (lambda (k) (- (* 2.0 k) 1.0))
             k))