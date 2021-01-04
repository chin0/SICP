#lang sicp
(define (cube x) (* x x x))

(define (square x) (* x x))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (croot-iter guess x)
  (if (good-enough? guess x)
      guess
      (croot-iter (improve guess x) x)))
;why inital guess value is 1.1????? (prevent an anomalous result for croot of -2???)
(define (croot x)
  (croot-iter 1.1 x))
  