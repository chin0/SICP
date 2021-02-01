#lang sicp
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree test-tree 10)

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree test-tree)

(define (tree-map func tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map func subtree)
             (func subtree))) tree))

(define (square-tree-map tree)
  (tree-map square tree))

(square-tree-map test-tree)
;(1 2 3) -> (() (3) (2) (2 3) (1) (2 3) (1 3) (1 2) (1 2 3))
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))
