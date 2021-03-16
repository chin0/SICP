#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

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

(define (make-joint account original-password password)
  (define (call-the-cops . args)
    "call the cops!")
  (define (dispatch p m)
    (if (eq? p password)
        (account original-password m)
        call-the-cops))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 50)
((peter-acc 'open-sesame 'withdraw) 50)
(define (make-accumulator initial)
  (lambda (x)
    (begin (set! initial (+ initial x))
           initial)))


(define (make-monitored f)
  (let ((number-of-called 0))
   (define (mf arguments)
    (if (eq? arguments 'how-many-calls?)
        number-of-called
        (begin (set! number-of-called (+ 1 number-of-called))
               (f arguments))))
    mf))

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

(define f
  (let ((state 0))
    (lambda (x)
      (define old-state state)
      (set! state x)
      old-state)))
      
