#lang sicp

; 각 부서 파일은 적어도 이름을 키값으로 가지는 레코드.
; 따라서 각 부서별로 이름을 인자로 주면 해당 레코드를 뱉는 프로시저를 작성해 put해줘야함. 
(define (get-record division file  name)
  ((get 'get-record division) file name))

(define (get-salary division record)
  ((get 'get-salary division) record))

;; 부서 파일 받으면 divison 뱉는거 하나 필요함.
;; 부서를 하나의 타입이라고 생각하면 될듯.
(define (find-employee-record name files)
  (define (iter filelist)
    (let ((result (get-record (division (car filelist)) (car filelist) name)))
      (cond ((null? filelist) false)
            ((not (null? result)) result)
            (else (iter (iter (cdr filelist)))))))
  (iter null files))