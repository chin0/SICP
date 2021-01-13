#lang sicp
(define (compose f g) (lambda (x) (f (g x))))

(define dx 0.00001)

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (square x) (* x x))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next)))
    )
  (try first-guess))

(define (exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp b (/ n 2))))
        (else (* b (exp b (- n 1))))))
(define (average a b)
  (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (nroot x n)
  (define (get-repeat-count c current)
    (if (>= current n)
        c
        (get-repeat-count (+ c 1) (* 2 current))))
  (fixed-point-of-transform (lambda (y) (/ x (exp y (- n 1))))
                            (repeat average-damp (get-repeat-count 0 1))
                            1.0))