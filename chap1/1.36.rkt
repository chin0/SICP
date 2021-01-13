#lang sicp
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next)))
    )
  (try first-guess))

(define (average a b) (/ (+ a b) 2.0))
(define (f)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 3.0))

(define (g)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 3.0))