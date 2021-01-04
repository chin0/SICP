#lang sicp
(define (square x) (* x x))
(define (abs x)
  (if (>= x 0) x (- x)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improved-enough? prev next)
  (< (abs (- prev next)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess improved x)
  (if (improved-enough? guess improved)
      improved
      (sqrt-iter2 improved
                 (improve improved x)
                 x)))
(define (sqrt2 x)
  (sqrt-iter2 1.0 (improve 1.0 x) x))

(define (improved-enough-alt guess x)
  (= (improve guess x) guess))

(define (sqrt-iter3 guess x)
  (if (improved-enough-alt guess x)
      guess
      (sqrt-iter3 (improve guess x) x)))
(define (sqrt3 x)
  (sqrt-iter3 1.0 x))