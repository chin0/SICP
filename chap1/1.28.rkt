#lang sicp
(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0 ))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (define (sqmod x)
    (if (and (= (remainder (square x) m) 1)
             (not (= x 1))
             (not (= x (- m 1))))
        0
        (remainder (square x) m)))
    
  (cond ((= exp 0) 1 )
        ((even? exp)
         (sqmod (expmod base (/ exp 2) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))