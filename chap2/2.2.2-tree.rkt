#lang sicp

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (deep-reverse x)
  (define (reverse-iter items result)
    (cond ((null? items) result)
          ((not (pair? items)) items)
          (else (reverse-iter (cdr items) (cons (reverse-iter (car items) nil) result)))))
  (reverse-iter x nil))

(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (balanced? mobile)
  (define (calc-torque branch)
    (* (total-weight (branch-structure branch)) (branch-length branch)))
  (cond ((null? mobile) #t)
        ((not (pair? mobile)) #t)
        ((not (= (calc-torque (left-branch mobile))
                 (calc-torque (right-branch mobile)))) #f)
        (else (and (balanced? (branch-structure (left-branch mobile)))
                   (balanced? (branch-structure (right-branch mobile)))))))

(define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8))))) 
(define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4)))))

(define x (list (list 1 2) (list 3 4)))
(define test-mobile (make-mobile (make-branch 3 3) (make-branch 1 9)))
(define test-mobile2 (make-mobile (make-branch 3 test-mobile) (make-branch 7 test-mobile)))