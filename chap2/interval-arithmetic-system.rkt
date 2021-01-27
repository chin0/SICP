#lang sicp
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval-improve x y)
  
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (>= x1 0) (>= x2 0) (>= y1 0) (>= y2 0)) (make-interval (* x1 y1) (* x2 y2)))
          ((and (>= x1 0) (>= x2 0) (< y1 0) (>= y2 0)) (make-interval (* x2 y1) (* x2 y2)))
          ((and (>= x1 0) (>= x2 0) (< y1 0) (< y2 0)) (make-interval (* x2 y1) (* x1 y2)))
          ((and (< x1 0) (>= x2 0) (>= y1 0) (>= y2 0)) (make-interval (* x1 y2) (* x2 y2)))
          ((and (< x1 0) (>= x2 0) (< y1 0) (< y2 0)) (make-interval (* x2 y1) (* x1 y1)))
          ((and (< x1 0) (< x2 0) (>= y1 0) (>= y2 0)) (make-interval (* x1 y2) (* x2 y1)))
          ((and (< x1 0) (< x2 0) (< y1 0) (>= y2 0)) (make-interval (* x1 y2) (* x1 y1)))
          ((and (< x1 0) (< x2 0) (< y1 0) (< y2 0)) (make-interval (* x2 y2) (* x1 y1)))
          (else
           (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4)))))))
        
(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
          (= (lower-bound y) 0)) (error "divide by zero") 
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent x)
  (let ((w (width x))
        (c (center x)))
    (* w (/ 100 c))))
(define t1 (make-interval 1 2))
(define t2 (make-interval 3 4))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define A (make-center-percent 3 0.000001))
(define B (make-center-percent 4 0.000001))
(div-interval A A)
(div-interval A B)