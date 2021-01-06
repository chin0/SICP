#lang sicp
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (define (f-iter a b current counter)
    (if (= counter 0)
        current
        (f-iter (+
                  (* 3 current)
                  (* 2 b)
                  (* a)) a b (- counter 1))))                 
  (f-iter 2 1 0 n))
    