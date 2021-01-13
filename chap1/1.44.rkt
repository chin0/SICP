#lang sicp
(define (compose f g) (lambda (x) (f (g x))))

(define dx 0.00001)

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (average a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (nth-smooth f n)
  (repeat smooth n))
  