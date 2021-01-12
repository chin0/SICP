#lang sicp
(define (filtered-accumulator p combinator init term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((p a) (iter (next a) (combinator (term a) result)))
          (else (iter (next a) result))))
  (iter a init))

(define (square x) (* x x))

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

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (prime-test n try)
  (cond ((= try 0) true)
        ((miller-rabin n) (prime-test n (- try 1)))
        (else false)))

(define (prime? n)
  (prime-test n 10))

(define (inc x) (+ x 1))
(define (id x) x)

(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (sum-of-square-prime a b)
  (filtered-accumulator prime? + 0 square a inc b))

(define (product-of-relative-prime n)
  (define (relative-prime? a) (= (gcd n a) 1))
  (filtered-accumulator relative-prime? * 1 id 1 inc (- n 1)))