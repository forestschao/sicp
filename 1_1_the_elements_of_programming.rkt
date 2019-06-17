#lang racket

; The Elements of Programming
; 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; 1.3
(define (sum-of-two-max-square a b c)
  (define (square a) (* a a))
  (define (min a b c)
    (cond ((and (<= a b) (<= a c)) a)
          ((and (<= b a) (<= b c)) b)
          (else c)))
  (- (+ (square a) (square b) (square c))
     (square (min a b c))))

; 1.7
(define delta 0.00001)

(define (good-enough? a b)
  (< (abs (- a b)) delta))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt x)
  (define (sqrt-iter old_guess guess x)
    (if (good-enough? old_guess guess)
        guess
        (sqrt-iter guess (improve guess x) x)))
  (sqrt-iter 1 (improve 1 x) x))

; 1.8
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) delta))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube-root guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root (improve-cube guess x) x)))