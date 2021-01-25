#lang sicp

(define (square x) (* x x))

(define (exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp b (/ n 2))))
        (else (* b (exp b (- n 1))))))

(define (cons a b)
  (* (exp 2 a) (exp 3 b)))

(define (naive-log b n count)
    (if (= (remainder n b) 0)
        (naive-log b (/ n b) (+ 1 count))
        count))

(define (car x)
  (naive-log 2 x 0))

(define (cdr x)
  (naive-log 3 x 0))
