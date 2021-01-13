#lang sicp
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

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (newtons-method g guess)
  (fixed-point-of-transform g newton-transform guess))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (square x) (* x x))
(define (average a b)
  (/ (+ a b) 2.0))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))