#lang sicp
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0) result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (get-e k)
  (+ 2 (cont-frac (lambda (x) 1.0)
             (lambda (x) (if (= (remainder x 3) 2)
                             (* 2 (/ (+ x 1) 3)) 1))
             k)))