#lang sicp
(define (cond-frac n d k)
  (define (recursive-iter i)
    (cond ((= i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (recursive-iter (+ i 1)))))))
  (recursive-iter 1))

(define (cond-frac-iter n d k)
  (define (iter i result)
    (if (= i 0) result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

         