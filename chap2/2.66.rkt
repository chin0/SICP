#lang sicp

(define (accumulate f initial items)
  (if (null? items)
      initial
      (f (car items) (accumulate f initial (cdr items)))))

(define (make-record key data) (list 'record key data))

(define (key record) (cadr record))

(define (data record) (caddr record))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (key (entry set))) true)
        ((< x (key (entry set)))
         (element-of-set? x (left-branch set)))
        ((> x (key (entry set)))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (key x) (key (entry set))) set)
        ((< (key x) (key (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (key x) (key (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (orderedlist->intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (orderedlist->intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2) (orderedlist->intersection-set (cdr set1) set2))
              ((< x2 x1) (orderedlist->intersection-set set1 (cdr set2)))))))

(define (orderedlist->union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (orderedlist->union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (orderedlist->union-set (cdr set1) set2)))
                 ((< x2 x1) (cons x2 (orderedlist->union-set set1 (cdr set2)))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))

(define (union-set s1 s2)
  (list->tree (orderedlist->union-set (tree->list s1) (tree->list s2))))

(define (intersection-set s1 s2)
  (list->tree (orderedlist->intersection-set (tree->list s1) (tree->list s2))))

(define t (make-tree (make-record 1 'data1) '() '()))

(define (lookup given-key set)
  (cond ((null? set) false)
        ((= given-key (key (entry set))) (entry set))
        ((< given-key (key (entry set)))
         (lookup given-key (left-branch set)))
        ((> given-key (key (entry set)))
         (lookup given-key (right-branch set)))))

(define test (accumulate adjoin-set t
            (list (make-record 2 'data2)
                  (make-record 3 'dsaf)
                  (make-record 4 'ssss)
                  (make-record 5 'a))))

(lookup 3 test)

