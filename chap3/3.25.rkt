#lang sicp

;use list as key and set compare function to equal?
(define (make-table)
  (let ((local-table (list '*table)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define t (make-table))
((t 'insert-proc) '(apple price) 3)
((t 'insert-proc) '(apple amount) 4)
((t 'insert-proc) '(banana price) 5)
((t 'insert-proc) '(apple price dest) 10)
((t 'lookup-proc) '(apple price))
((t 'lookup-proc) '(apple price dest))