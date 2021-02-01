#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves2 t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (subtree)
                     (if (pair? subtree) (count-leaves2 subtree) 1)) t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define x (list (list 1 2) (list 3 4)))
(accumulate-n + 0 x)

(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 7 8 9 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rowvec) (accumulate (lambda (colvec list)
                                        (cons (dot-product rowvec colvec) list))
                                      nil
                                      cols))
         m)))

(define test-square (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))