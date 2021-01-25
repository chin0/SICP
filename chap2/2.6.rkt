#lang sicp
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(two one)

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

;(one f) -> (lambda (x) (f x))
;((two f) x)-> (lambda (x) (f (f x)) -> (f (f x))
;(add m n) -> (lambda (f) (lambda (x) ((m f) ((n f) x)
;((one f) (two f)) = ((lambda (x) (f x)) (lambda (x) (f (f (x)))
;((one f) ((two f) x)) = (f (f (f x)))

; (f (lambda (x) (f (f (x)))))

