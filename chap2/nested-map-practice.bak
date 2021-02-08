#lang sicp
(define (square x) (* x x))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

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

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (+ start 1) end))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
                  