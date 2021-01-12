#lang sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (* (f (+ a (* k h))) (cond ((or (= k 0) (= k n)) 1)
                                          ((even? k) 4)
                                          (else 2))))
  (* (/ h 3.0) (sum y 0 inc n)))