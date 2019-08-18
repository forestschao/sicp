#lang sicp
; 2.53
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else
         (memq item (cdr x)))))

; 2.54
(define (my-equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else false)))

; 2.55
; transform into (quote (quote abracadabra))
; (cdr ''abracadabra)

; 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((in-sum? exp)
         (make-in-sum (deriv (in-addend exp) var)
                      (deriv (in-augend exp) var)))
        ((in-product? exp)
         (make-in-sum (make-in-product (in-multiplier exp)
                                       (deriv (in-multiplicand exp) var))
                      (make-in-product (deriv (in-multiplier exp) var)
                                       (in-multiplicand exp))))
        ((exponentiation? exp)
         (let ((e (exponent exp))
               (b (base exp)))
           (cond ((=number? e 0) 0)
                 ((=number? e 1) (deriv b var))
                 (else
                  (make-product (make-product e
                                              (make-exponentiation b (- e 1)))
                                (deriv b var))))))
       ; ((singleton? exp) (deriv (car exp) var))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define addend cadr)
(define (augend exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cons '+ (cddr exp))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define multiplier cadr)
(define (multiplicand exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cons '* (cddr exp))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else
         (list '** b e))))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (car exp) '**)))

(define base cadr)
(define exponent caddr)

; 2.57
(define (singleton? exp)
  (and (pair? exp)
       (null? (cdr exp))))

(define (simplify exp)
  (if (singleton? exp)
      (car exp)
      exp))

(define (extract-before item x)
  (define (iter rest result)
    (cond ((null? rest) false)
          ((eq? item (car rest))
           (simplify (reverse result)))
          (else
           (iter (cdr rest) (cons (car rest) result)))))
  (iter x '()))

(define (extract-after item x)
  (simplify (cdr (memq item x))))

(define (make-in-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (in-sum? x)
  (and (pair? x)
       (memq '+ x)))

(define (in-addend exp)
  (extract-before '+ exp))

(define (in-augend exp)
  (extract-after '+ exp))


(define (make-in-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (in-product? x)
  (and (pair? x)
       (memq '* x)))

(define (in-multiplier exp)
  (extract-before '* exp))

(define (in-multiplicand exp)
  (extract-after '* exp))

