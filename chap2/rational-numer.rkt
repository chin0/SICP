#lang sicp
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (abs x)
  (if (< x 0) (- x) x))

(define (xor pred1 pred2)
  (and (or pred1 pred2)
       (not (and pred1 pred2))))

;2.1 solution included function
;make xor combinator. and x < 0 xor y <0, then x is minus abs x
(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (n (if (xor (< n 0) (< d 0)) (- (abs n)) (abs n)))
        (d (if (< d 0) (- d) d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
