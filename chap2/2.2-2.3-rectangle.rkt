#lang sicp
(define (rect-area r)
  (* (rectangle-height r) (rectangle-width r)))

(define (rect-peri r)
  (+ (* 2 (rectangle-height r)) (* 2 (rectangle-width r))))

;=============================
;another representation is two line or three point
(define (make-rectangle p1 height width)
  (cons (make-segment p1 (make-point (x-point p1)
                                     (+ (y-point p1) height)))
        (make-segment p1 (make-point (+ (x-point p1) width)
                                     (y-point p1)))))
(define (height-line r)
  (car r))
(define (width-line r)
  (cdr r))
(define (rectangle-height r)
  (- (y-point (end-segment (height-line r)))
     (y-point (start-segment (height-line r)))))
                                     
(define (rectangle-width r)
  (- (x-point (end-segment (width-line r)))
     (x-point (start-segment (width-line r)))))
                                     
;=============================
(define (midpoint-segment line)
  (let ((start-x (x-point (start-segment line)))
        (end-x (x-point (end-segment line)))
        (start-y (y-point (start-segment line)))
        (end-y (y-point (end-segment line))))
    (make-point (abs start-x end-x) (abs start-y end-y))))
;============================
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

;==============================
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
;====================
(define (abs x y)
  (/ (+ x y) 2))

(define test-point (make-point 2 2))
(define test-rect (make-rectangle test-point 3 2))
(rect-area test-rect)
(rect-peri test-rect)