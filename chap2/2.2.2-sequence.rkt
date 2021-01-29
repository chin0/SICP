#lang sicp
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define odds (list 1 3 5 7 9))
(length odds)

(append squares odds)

(define (last-pair list1)
  (if (null? (cdr list1))
      list1
      (last-pair (cdr list1))))

(define (reverse list1)
  (define (iter iter-list current-list)
    (if (null? iter-list)
        current-list
        (iter (cdr iter-list) (cons (car iter-list) current-list))))
  (iter list1 nil))

(define (count-change amount)
  (cc amount 5))

(define (no-more? coin-values)
  (null? coin-values))

(define (expect-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (expect-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(define (same-parity . values)
  (define (sp-iter lists p)
    (cond ((null? lists) nil)
          ((p (car lists)) (cons (car lists) (sp-iter (cdr lists) p)))
          (else (sp-iter (cdr lists) p))))
  (if (even? (car values)) (sp-iter values even?) (sp-iter values odd?)))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 2 3 1 4 2))