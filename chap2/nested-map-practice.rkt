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

(define (enumerate-interval-except-k start end . klist)
  (filter (lambda (i) (not (k-in-list? klist i)))
          (enumerate-interval start end)))

(define (k-in-list? list k)
  (cond ((null? list) #f)
        ((= (car list) k) #t)
        (else (k-in-list? (cdr list) k))))

(define (gen-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval-except-k 1 n i j)))
                  (enumerate-interval-except-k 1 n i)))
           (enumerate-interval 1 n)))

(define (triples-sum-s n s)
  (define (sum-is-s? triples) (= s (accumulate + 0 triples)))
  (filter sum-is-s? (gen-triples n)))

;board abstraction
;============= NQUEEN 2.42-2.43 =================
(define test-board (list (list 1 1) (list 2 1) (list 3 1)))

(define (make-position rownum colnum)
  (cons rownum colnum))

(define (row position)
  (car position))

(define (col position)
  (cdr position))

(define empty-board nil)

(define (adjoin-position rownum colnum board)
  (append board (list (make-position rownum colnum))))

(define (diagonal? p1 p2)
  (= (abs (- (row p1) (row p2))) (abs (- (col p1) (col p2)))))

(define (horizontal? p1 p2)
  (= (row p1) (row p2)))

(define (position-safe? p1 p2)
  (not (or (horizontal? p1 p2) (diagonal? p1 p2))))

(define (safe? colnum positions)
  (let ((kth-queens (list-ref positions (- colnum 1)))
        (other-queens (filter (lambda (q)
                                (not (= colnum (col q))))
                              positions)))
    (define (iter q board)
      (or (null? board)
          (and (position-safe? q (car board))
               (iter q (cdr board)))))
    
    (iter kth-queens other-queens)))
                                     

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))