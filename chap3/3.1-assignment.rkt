#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 30)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 30)
(new-withdraw 40)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 200))

(W1 30)
(W2 100)

(define (make-account balance password)
  (define incorrect-numbers 0)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (incorrect-message . args)
    (begin (set! incorrect-numbers (+ 1 incorrect-numbers))
                 "Incorrect password"))

  (define (call-the-cops . args)
    "call the cops!")
  
  (define (dispatch p m)
    (if (>= incorrect-numbers 7)
        call-the-cops
        (if (eq? p password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m)))
            incorrect-message)))
    dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

(define (make-accumulator initial)
  (lambda (x)
    (begin (set! initial (+ initial x))
           initial)))

(define A (make-accumulator 5))
(A 10)
(A 10)

(define (make-monitored f)
  (let ((number-of-called 0))
   (define (mf arguments)
    (if (eq? arguments 'how-many-calls?)
        number-of-called
        (begin (set! number-of-called (+ 1 number-of-called))
               (f arguments))))
    mf))

(define s (make-monitored sqrt))
(s 100)
(s 144)
(s 'how-many-calls?)

(define random-init 1231231)

(define (rand-update x)
  (remainder (+ (* x 25214903917) 1) 281474976710656))

(define rand
  (let ((x random-init))
    (define (reset value)
      (set! x value))
    (define (generate)
      (set! x (rand-update x))
      x)
    (lambda (message)
      (cond ((eq? message 'generate) (generate))
            ((eq? message 'reset) reset)))))

(rand 'generate)
((rand 'reset) 32)
(rand 'generate)
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;(estimate-pi 10000000)

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
        (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (integral-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ((square-area (* (abs (- x1 x2)) (abs (- y1 y2))))) 
    (* (monte-carlo trials integral-test) square-area)))

(estimate-integral (lambda (x y)
                     (<= (+ (square x) (square y)) 1)) -100 100 -100 100 1000000)

(define peter-acc (make-account 100 'password))
(define paul-acc peter-acc)
((peter-acc 'password 'withdraw) 10)
((paul-acc 'password 'deposit) 30)