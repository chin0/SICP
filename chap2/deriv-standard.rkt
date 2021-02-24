#lang sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? a b)
  (cond ((eq? a b) #t)
        ((and (and (pair? a) (pair? b))
              (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))) #t)
        (else #f)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (cond ((not (pair? x)) #f)
        ((null? x) #f)
        ((eq? (car x) '+) #t)
        (else (sum? (cdr x)))))

(define (augend s)
  (define (iter result current)
    (if (or (null? current) (eq? (car current) '+))
        (if (= (length result) 1) (car result) result)
        (iter (append result (list (car current))) (cdr current))))
  (iter nil s))
;(define (augend s) (car s))

(define (addend s)
  (if (or (null? s) (eq? (car s) '+))
      (if (= (length (cdr s)) 1) (car (cdr s)) (cdr s))
      (addend (cdr s))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplicand p) (car p))

(define (multiplier p)
  (cond ((= (length (cddr p)) 1) (caddr p))
        ((sum? (cddr p)) (augend (cddr p)))
        (else (cddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (square x)
  (* x x))

(define (exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp b (/ n 2))))
        (else (* b (exp b (- n 1))))))

(define (make-exponentiation s exponent)
  (cond ((and (number? s) (number? exponent)) (exp s exponent))
        ((=number? exponent 0) 1)
        ((=number? exponent 1) s)
        (list '** s exponent)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                       (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;some tests
(deriv '(x + y + z) 'x) ; 1
(deriv '(3 * 2 * x) 'x) ; 6
(deriv '(3 * 2 + x) 'x) ; 1
(deriv '((3 * x + 4) * x) 'x)
;(augend '(x * 2 + y * 4 + 3 * 2))
;(addend '(x * 2 + y * 4 + 3 * 2))