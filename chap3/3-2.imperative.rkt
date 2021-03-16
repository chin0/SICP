#lang sicp
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

(define (visited-list)
  (let ((visited-pairs '()))
    (define (visit? p)
      (memq p visited-pairs))
    (define (add-pairs p)
      (set! visited-pairs (cons p visited-pairs)))
    (define (dispatch m)
      (cond ((eq? m 'add) add-pairs)
            ((eq? m 'visit?) visit?)
            (else (error "not valid message"))))
    dispatch))

;3.17 implementation
(define (count-pairs x)
  (let ((v (visited-list)))
    (define (iter current)
      (if (or (not (pair? current)) ((v 'visit?) current))
          0
          (begin ((v 'add) current)
                 (+ 1
                    (iter (car current))
                    (iter (cdr current))))))
    (iter x)))

(define p1 (cons 'a '()))
(define p2 (cons 'b '()))
(define p3 (cons 'c '()))
(set-cdr! p2 p3)
(set-cdr! p1 p2)
(count-pairs p1)