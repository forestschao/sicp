#lang racket
; 2.73
; a. The method turn the operator into data-directed method.
; That each operator has the same form.
; number? and variable? can't be written in the
; same way since they are in different forms.

; b.
(define (attach-tag tag content)
  (cons tag content))

(define (make-sum a b)
  (list '+ a b))
(define (make-product a b)
  (list '* a b))
(define (make-exponent b e)
  (list '** b e))

(define (put op type proc)
  (list op type proc))

(define (install-sum-package)
  (define (deriv args var)
    (make-sum (deriv (car args) var)
              (deriv (cadr args) var)))

  (put 'deriv '+ deriv)
  'done)

(define (install-product-package)
  (define (deriv args var)
    (let ((multiplier (car args))
          (multiplicand (cadr args)))
      (make-sum
       (make-product multiplier
                     (deriv multiplicand var))
       (make-product (deriv multiplier var)
                     (multiplicand)))))
  (put 'deriv '* deriv)
  'done)

(define (install-exponent-package)
  (define (deriv args var)
    (let ((base (car args))
          (exponent (cadr args)))
      (make-product
       (make-product base
                     (make-exponent base (- exponent 1)))
       (deriv exponent var))))
  (put 'deriv '** deriv)
  'done)

; d. Change the put order
; For example: (put '+ 'deriv deriv) in the sum-package

; 2.74
;
;      division:
;               |    financial   |    IT   | ....
;   get-record
;   get-salary

(define (get a . b) a)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (get-record file employee)
  ((get 'get-record (type-tag file)) employee))

(define (get-salary employee)
  ((get 'get-salary (type-tag employee)) employee))

(define (find-employee-record employee files)
  (if (null? files)
      (error "Can't find the employee" employee)
      (let ((result (get-record (car files))))
        (if (null? result)
            (find-employee-record employee (cdr files))
            result))))

; 2.75
(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)))
  dispatch)

; 2.76
;        dispatch: Easy for add operation.
;   data-directed: Easy for add type and operation.
; message-passing: Easy for add type.
        