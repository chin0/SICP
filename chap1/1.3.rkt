#lang sicp
(define (square x) (* x x))
(define (squareSum x y) (+ (square x) (square y)))

(define (max a b)
  (if (> a b) a b))

(define (max-three a b c)
  (max a (max b c)))

(define (mySumOfLargestTwo a b c)
  (cond ((= a (max-three a b c)) (squareSum a (max b c)))
        ((= b (max-three a b c)) (squareSum b (max a c)))
        ((= c (max-three a b c)) (squareSum c (max c a)))))

(define (solutionSumOfLargestTwo a b c)
  (cond ((and (>= (+ a b) (+ b c)) (>= (+ a b) (+ a c))) (squareSum a b))
        ((and (>= (+ a c) (+ b c)) (>= (+ a c) (+ a b))) (squareSum a c))
        (else (squareSum b c))
        )
  )