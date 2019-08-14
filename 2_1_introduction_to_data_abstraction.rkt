#lang racket
(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y) 
  (make-rat (- (* (numer x) (denom y))
                  (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define numer car)
(define denom cdr)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cons ((if (>= (* n d) 0) + -) (/ (abs n) g))
          (/ (abs d) g))))

; 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (begin
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (displayln ")")))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment line)
  (let ((start (start-segment line))
        (end (end-segment line)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

; 2.3
(define (make-rect a b)
  (cons a b))

(define (a-rect rec)
  (car rec))

(define (b-rect rec)
  (cdr rec))

(define (perimeter rec)
  (* 2
     (+ (a-rect rec)
        (b-rect rec))))

(define (area rec)
  (* (a-rect rec)
     (b-rect rec)))

; 2.4 This is a little surprising
(define (ncons x y)
  (lambda (m) (m x y)))

(define (ncar z)
  (z (lambda (p q) p)))

(define (ncdr z)
  (z (lambda (p q) q)))

; 2.5
; assume p = 2^a1 * 3^b2
;        q = 2^a2 * 3^b2
; If p = q
;  <=> a1 = a2 and b1 = b2
(define (pcons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (pcar l)
  (define (iter v result)
    (if (> (remainder v 2) 0)
        result
        (iter (/ v 2) (+ result 1))))
  (iter l 0))

(define (pcdr l)
  (if (> (remainder l 3) 0)
      0
      (+ 1 (pcdr (/ l 3)))))

; 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (church-plus a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))

(define (church-plus2 a b)
  ((a add-1) b))

(define (church-mul a b)
  (lambda (f) (a (b f))))

(define (church->int church)
  ((church (lambda (x) (+ x 1))) 0))

(define (int->church n)
  (if (= n 0)
      zero
      (add-1 (int->church (- n 1)))))

; 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))

(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
        (p2 (* (lower-bound a) (upper-bound b)))
        (p3 (* (upper-bound a) (lower-bound b)))
        (p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

; 2.9
(define (width i)
  (/ (- (upper-bound i) (lower-bound i))
     2))

(define (div-interval a b)
  (mul-interval a
                (make-interval (/ 1.0 (upper-bound b))
                               (/ 1.0 (lower-bound b)))))
; Ignore /2 since it applies to both
; sides of the equations
; sum:
; w1 = y1 - x1
; w2 = y2 - x2
; ws = (y1 + y2) - (x1 + x2)
;    = (y1 - x1) + (y2 - x2)
;    = w1 + w2
; sub:
; ws = (y1 - x2) - (x1 - y2)
;    = (y1 - x1) + (y2 - x2)
;    = w1 + w2

; 2.10
; I think span 0 is not an error.
; However, it leads to trouble if the end point is zero.
(define (div-interval-check a b)
  (if (or (= (lower-bound b) 0)
          (= (upper-bound b) 0))
      (error "Divide by zero -- DIV INTERVAL")
      (mul-interval
        a
        (make-interval (/ 1.0 (upper-bound b))
                       (/ 1.0 (lower-bound b))))))

; 2.11
(define (mul-interval-fast x y)
  (let ((p1 (lower-bound x))
        (q1 (upper-bound x))
        (p2 (lower-bound y))
        (q2 (upper-bound y)))
    (cond ((>= p1 0)
           (cond ((>= p2 0) (make-interval (* p1 p2) (* q1 q2)))
                 ((<= q2 0) (make-interval (* p2 q1) (* p1 q2)))
                 (else (make-interval (* p2 q2) (* q2 q2)))))
          ((<= q1 0)
           (cond ((>= p2 0) (make-interval (* p1 q2) (* p2 q1)))
                 ((<= q2 0) (make-interval (* q1 q2) (* p1 p2)))
                 (else (make-interval (* p1 q2) (* p1 p2)))))
          (else
           (cond ((>= p2 0) make-interval (* p1 q2) (* q1 q2))
                 ((<= q2 0) make-interval (* p2 q1) (* p2 p1))
                 (else (make-interval (min (* p1 q2) (* p2 q1))
                                      (max (* p1 p2) (* q1 q2)))))))))

; 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

; 2.13
; p1 = (y1 - x1) / (y1 + x1)
; p2 = (y2 - x2) / (y2 + x2)
; p1 + p2 = (2 * (y1y2 - x1x2)) / (x1x2 + y1y2 + x2y1 + x1y2)
;      pc =      (y1y2 - x2x2) / (x1x2 + y1y2)

;2.14 ~ 2.16
(define R1 (make-center-percent 5 0.1))
(define R2 (make-center-percent 6 0.05))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
                  
(define p1 (par1 R1 R2))

(define p2 (par2 R1 R2))

(define (print-interval i)
  (begin
    (display "[")
    (display (lower-bound i))
    (display ", ")
    (display (upper-bound i))
    (displayln "]")))

(print-interval p1)
(print-interval p2)

;Well, IDK
             
                                    
          