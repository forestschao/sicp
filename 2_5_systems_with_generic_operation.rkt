#lang sicp
; 2.77
; The first apply generic returns magnitude.
; The magnitude itself is a generic function to call
; the magnitude of rectangular or polar.

; 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag "scheme-number")
      contents
      (cons type-tag contents)))


(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bag tagged datum -- CONTENTS" datum))))

; 2.79, 2.80, 2.83, 2.85
(define (get op types) op)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                  (list op type-tags))))))

(define (put op types) op)

(define (install-scheme-number-package)
  (define (raise a)
    ((get 'make 'rational) a 1))
  (put 'equ? '(scheme-number, scheme-number) =)
  (put 'zero? 'scheme-number (lambda(x) (= x 0)))
  (put 'raise 'scheme-number raise)
  'done)

(define (install-rational-package)
  (define numer car)
  (define denom cdr)
  (define (equ? a b)
    (= (* (numer a) (denom b))
       (* (numer b) (denom a))))
  (define (raise a)
    ((get 'make-from-real-imag 'complex) a 0))
  (put 'equ? '(rational, rational) equ?)
  (put 'zero? 'rational (lambda (x) (= (numer x) 0)))
  (put 'raise 'rational raise)
  (put 'project 'rational (lambda (x) (round (/ (numer x)
                                                (denom x)))))
  'done)

(define (install-complex-package)
  (define real-part car)
  (define imag-part cdr)
  (define (equ? a b)
    (and (= (real-part a) (real-part b))
         (= (imag-part a) (imag-part b))))
  (define (zero? a)
    (and (= (real-part a) 0)
         (= (imag-part a) 0)))
  (define (project a)
    ((get 'make 'rational) (real-part a) 1))
  (put 'equ? '(complex, complex) equ?)
  (put 'zero? 'complex zero?)
  'done)

; 2.81
; a. This will leads to infinite loop.
; b. It is unnecessary to do so.
; If there is already with the operation under the type,
; then it will be called in the first apply.
; Otherwise, even type is coerced to itself, it still
; can't run.
(define (get-coercion t1 t2) t1)
(define (apply-generic-type op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (= type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; 2.82
(define (coercion-args args type)
  (if (null? args)
      '()
      (let ((first (car args)))
        (let ((first-type (type-tag first)))
          (if (eq? first-type type)
              (cons first
                    (coercion-args (cdr args) type)))
              (let ((t1->t (get-coercion first-type type)))
                (if (t1->t)
                    (cons (t1->t first)
                          (coercion-args (cdr args) type))
                    '()))))))
                 
(define (apply-generic-multi-args op . args)
  (define (try-type types)
    (if (null? types)
        (error "No method for these types"
               (list op args))
        (let ((updated-args (coercion-args args (car types))))
          (if (null? updated-args)
              (try-type (cdr types))
              (apply-generic op updated-args)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-type type-tags)))))

; 2.84
(define (eq-type? arg1 arg2)
  (eq? (type-tag arg1)
       (type-tag arg2)))

(define (higher? arg1 arg2)
  (define (raise-type arg)
    (let ((higher (get 'raise (type-tag arg))))
      (if higher
          (higher arg2)
          '())))
  (define (iter arg2)
    (cond ((null? arg2) false)
          ((eq-type? arg1 arg2) true)
          (else (iter (raise-type arg2)))))
  (iter (raise-type arg2)))
          
(define (apply-generic-raise op . args)
  (define (highest-type args)
    (define (iter args result)
      (cond ((null? args) result)
            ((higher? (car args) result)
             (iter (cdr args) (car args)))
            (else
             (iter (cdr args) result))))
    (iter (cdr args) (car args)))
  
  (define (coercion-to-target arg target)
      (if (eq-type? arg target)
          arg
          (coercion-to-target
           (get 'raise (type-tag arg) arg)
           target)))
  
  (define (coercion-highest args)
    (let ((highest (highest-type args)))
      (define (to-target args)
        (if (null? args)
            '()
            (cons (coercion-to-target (car args) highest)
                  (to-target (cdr args)))))
      (to-target args)))

  (define (different-types? args)
    (define (iter args arg)
      (cond ((null? args) false)
            ((eq-type? (car args) arg)
             (iter (cdr args) arg))
            (else true)))
    (iter (cdr args) (car args)))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (drop (proc op (map contents args))))
            ((different-types? args)
             (drop (apply-generic-raise op (coercion-highest args))))
            (else
             (error "No method for these types"
                    (list op type-tags)))))))

(define (raise x) (apply-generic 'raise x))

(define (drop n)
  (let ((project (get 'project (type-tag n))))
    (if project
        (if (eq? (raise (project n)) n)
            (drop (project n))
            n)
        n)))

; 2.86
; change the + in the complex number to be the generic function
; add, sub, mul, and div.