#lang sicp

;implement doubly linked list (memory leak,,?)

(define (new-cell item) (cons item (cons nil-cell nil-cell)))

(define nil-cell (cons '() (cons '() '())))

(define (item cell) (car item))

(define (next-ptr cell) (cadr cell))

(define (prev-ptr cell) (cddr cell))

(define (null-cell? cell) (eq? cell nil-cell))

(define (set-item! cell item) (set-car! cell item))

(define (set-next-ptr! cell item) (set-car! (cdr cell) item))

(define (set-prev-ptr! cell item) (set-cdr! (cdr cell) item))


(define (cons-cell c1 c2)
  (set-next-ptr! c1 c2)
  (set-prev-ptr! c2 c1)
  c1)

; implement deque using doubly linked list

(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (or (null-cell? (rear-ptr deque)) (null-cell? (front-ptr deque))))

(define (make-deque) (cons nil-cell nil-cell))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (new-cell item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-next-ptr! (rear-ptr deque) new-pair)
           (set-prev-ptr! new-pair (rear-ptr deque))
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (new-cell item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-next-ptr! new-pair (front-ptr deque))
           (set-prev-ptr! (front-ptr deque) new-pair)
           (set-front-ptr! deque new-pair)
           deque))))

(define (delete-front-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue" deque))
        (else
         (set-front-ptr! deque (next-ptr (front-ptr deque)))
         (set-prev-ptr! (front-ptr deque) nil-cell)
         deque)))

(define (delete-rear-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue" deque))
        (else
         (set-rear-ptr! deque (prev-ptr (rear-ptr deque)))
         (set-next-ptr! (rear-ptr deque) nil-cell)
         deque)))

(define d1 (make-deque))
(front-insert-deque! d1 'a)
(front-insert-deque! d1 'b)
(front-insert-deque! d1 'c)
(rear-ptr (delete-rear-deque! d1))
(delete-rear-deque! d1)
(delete-rear-deque! d1)

(rear-ptr d1)