#lang sicp
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;TODO: 3.17 implementation

(define (count-pairs x)
  ;적절한 데이터구조 만들어서 해당 pair가 중복계산인지 아닌지 체크.
  ;그래프 순회하듯이 하면 될듯. 
  (let ((visited '()))
