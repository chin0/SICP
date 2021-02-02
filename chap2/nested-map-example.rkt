#lang sicp
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

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove x seq)
  (filter (lambda (items) (not (= x items))) seq))