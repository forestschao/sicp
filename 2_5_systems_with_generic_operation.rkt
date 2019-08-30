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

; 2.87
(define (add . x) ((apply-generic 'add) x))
(define (negate x) ((apply-generic 'negate) x))

(define (sub p1 p2)
  (add p1 (negate p2)))

(define (install-polynomial-package)
  (define make-poly cons)
  (define variable car)
  (define term-list cadr)

  (define order car)
  (define coeff cadr)

  (define (order-less? o1 o2)
    (cond ((null? o1) false)
          ((< (car o1) (car o2)) true)
          ((> (car o1) (car o2)) false)
          ((order-less? (cdr o1) (cdr o2)))))

  (define (order-plus o1 o2)
    (map + o1 o2))

    (define (element-of? v L)
    (cond ((null? L) false)
          ((eq? v (car L)) true)
          (else (element-of? v (cdr L)))))
        
  (define (expand-variables v1 v2)
    (cond ((null? v2) v1)
          ((element-of? (car v2) v1)
           (expand-variables v1 (cdr v2)))
          (else
           (expand-variables (cons (car v2) v1)
                             (cdr v2)))))

  (define (expand-term origin all)
    (lambda (t)
      (make-term
       (map (lambda (v)
              (define (iter variables orders)
                (cond ((null? variables) 0)
                      ((eq? (car variables) v) (car orders))
                      (else (iter (cdr variables)
                                  (cdr orders)))))
              (iter origin (order t)))
            all)
       (coeff t))))

  (define (expand p1 all)
    (make-poly all
               (sort
                (map (expand-term (variable p1) all) (term-list p1)))))

  (define (sort terms)
    (define (sort-one curr terms)
      (cond ((null? terms) (list curr))
            ((order-less? (order curr) (order (car terms)))
             (cons (car terms) (sort-one curr (cdr terms))))
            (else
             (cons curr terms))))
    (define (sort-all origin sorted)
      (if (null? origin)
          sorted
          (sort-all (cdr origin)
                    (sort-one (car origin)
                              sorted))))
    (sort-all terms '()))
  
  (define sparse-term-list term-list)
  (define (dense-term-list p)
    (define (iter terms curr result)
      (cond ((< curr 0) result)
            ((null? terms)
             (iter terms (- curr 1) (cons 0 result)))
            ((= (order (car terms)) curr)
             (iter (cdr terms)
                   (- curr 1)
                   (cons (order (car terms)) result)))
            (else
             (iter terms (- curr 1) (cons 0 result)))))
    (iter (term-list p) (order (car term-list p)) '()))
  
  (define (make-term order coeff)
    (list order coeff))
  
  (define (=zero? poly)
    (define (iter terms)
      (cond ((null? terms) true)
            ((= (car terms) 0) (iter (cdr terms)))
            (else false)))
    (iter (term-list poly)))

  (define (negate poly)
    (make-poly (variable poly)
               (map negate (term-list poly))))
  
  'done)

(define same-variable? eq?)
(define mul *)

;(define (install-polynomial-dense-package)
  (define make-poly cons)
  (define variable car)
  (define term-list cadr)

  (define empty-list? null?)
  (define empty-termlist? null?)
  (define the-empty-list '())
  (define the-empty-termlist '())
  (define rest-terms cdr)
  (define order car)
  (define coeff cadr)
  (define first-term car)
  (define div /)
  (define (adjoin-term term term-list)
    (cons term term-list))

  (define dense-term-list term-list)

  (define (make-term order coeff)
    (list order coeff))

  (define (sparse-term-list p)
    (define (iter terms order result)
      (if (null? terms)
          result
          (iter (cdr terms)
                (+ order 1)
                (cons (make-term order (car terms))
                      result))))
    (iter (term-list p) 0 '()))



  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (dense-term-list p1)
                              (dense-term-list p2)))
        ;(let ((all-variables (expand-variables (variable p1)
        ;                                       (variable p2))))
        ;  (add-poly (expand p1 all-variables)
        ;            (expand p2 all-variables)))))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (add-terms t1 t2)
    (cond ((null? t1) t2)
          ((null? t2) t1)
          (else
           (cons (+ (car t1) (car t2))
                 (add-terms (cdr t1) (cdr t2))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (dense-term-list p1)
                              (dense-term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
 ; (define (mul-terms t1 t2)
 ;   (if (null? t1)
 ;       '()
 ;       (add-terms (mul-term-by-all-terms (first-term t1) t2)
 ;                  (cons 0 (mul-terms (cdr t1) t2)))))
 ; (define (mul-term-by-all-terms t1 L)
 ;   (if (null? L)
 ;       '()
 ;       (map (lambda (x) (mul t1 x)) L)))

  (define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))
  ; 2.91
  (define (div-terms L1 L2)
  (if (empty-list? L1)
      (list (the-empty-list) (the-empty-list))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-list) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (sub L1
                                     (mul-term-by-all-terms (make-term new-o new-c) L2))
                                L2)))
                (list (cons (make-term (new-o new-c))
                            (car rest-of-result))
                      (cadr (rest-of-result)))))))))
   (define (gcd-terms a b)
     (if (empty-list? b)
         a
         (gcd-terms b (cadr (div-terms a b)))))

;  'done)

; 1 + 2x + 3x^2
; 4x + 5x^3
; 4x + 8x^2 + 17x^3 + 10x^4 + 15x^5

; 5x + 2
; (((1) 5) ((0) 2))
; xy + 1
; (((1 1) 1) ((0 0) 1))
; 2x^2 + 3xy + 4z
; (((1 0 0) 2) ((1 1 0) 3) ((0 0 1) 4))
(define p1 '((x) (((1) 5) ((0) 2))))
(define p2 '((x y) (((1 1) 1) ((0 0) 1))))
(define p3 '((x y z) (((1 0 0) 2) ((1 1 0) 3) ((0 0 1) 4))))

(define l1 '((x) ((4 1) (3 -1) (2 -2) (1 2))))
(define l2 '((x) ((3 1) (1 -1))))


