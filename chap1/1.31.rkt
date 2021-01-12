#lang sicp

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a))))
    )
  (iter a 1))

(define (recursive-prod term a next b)
  (if (> a b)
      1
      (* (term a) (recursive-prod term (next a) next b))))
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (pi-product n)
  (define (s x) (+ 3 (* 2 x))) 
  (define (nth-term n)
    (/ (* (+ (s n) 1) (- (s n) 1)) (square (s n)))) 
  (product nth-term 0.0 inc n))
        