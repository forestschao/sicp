#lang racket
; 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x 1))
  (define (simpson-term k)
    (let ((y (f (+ a (* k h)))))
      (cond ((or (= k 0) (= k n)) y)
            ((= (remainder k 2) 0) (* 2 y))
            (else (* 4 y)))))
  (* (/ h 3)
     (sum simpson-term 0 next n)))

; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31
; a
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x)
           1
           (lambda (x) (+ x 1))
           n))

(define (pi-stream n)
  (* 4
     (product (lambda (x)
                (/ (* (- x 1) (+ x 1))
                   (* x x)))
              3
              (lambda (x) (+ x 2))
              (+ (* 2 n) 1))))

; 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate-rec + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

; 1.33
(define (id x) x)
(define (next x) (+ x 1))
(define (square x) (* x x))

(define (filtered-accumulate combiner null-value term a next b predicate?)
  (define (iter a result)
    (if (> a b) result
        (if (predicate? a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a null-value))

(define (prime? x)
  (define (iter a)
    (cond ((> (square a) x) #t)
          ((= (remainder x a) 0) #f)
          (else (iter (+ a 1)))))
  (iter 2))

(define (sum-of-primes a b)
  (filtered-accumulate + 0 square a next b prime?))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (product-of-less-n n)
  (filtered-accumulate * 1 id 1 next n (lambda (x) (= (gcd n x) 1))))

; 1.34
; Comple error. Since 2 is not a function which accepts a number.
(define (f g) (g 2))

; 1.35
(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (iter guess)
    (let ((new-guess (f guess)))
      (if (close-enough? new-guess guess)
          new-guess
          (iter new-guess))))
  (iter first-guess))

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

; 1.36
(define (show-fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (try guess)
    (begin
      (displayln guess)
      (let ((new-guess (f guess)))
        (if (close-enough? new-guess guess)
            new-guess
            (try new-guess)))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

;(define x-to-x1
;  (show-fixed-point (lambda (x) (/ (log 1000)
;                                   (log x)))
;                    2.0))
;(define x-to-x2
;  (show-fixed-point (lambda (x) (average x
;                                         (/ (log 1000)
;                                            (log x))))
;                    2.0))

; 1.37
(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0) result
        (iter (- k 1)
              (/ (n k)
                 (+ (d k) result)))))
  (iter (- k 1)
        (/ (n k) (d k))))

(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k) (/ (n k) (d k))
        (/ (n i)
           (+ (d i) (rec (+ i 1))))))
  (rec 1))


; 1.38
(define (euler-d index)
  (let ((remain (remainder index 3)))
    (if (or (= remain 1) (= remain 0)) 1
        (* 2 (+ (quotient index 3) 1)))))

(define (show k)
  (define (iter n)
    (if (> n k)
        (displayln "done")
        (begin
          (displayln (euler-d n))
          (iter (+ n 1)))))
  (iter 1))

; (displayln (cont-frac (lambda (x) 1.0) euler-d 100))
; (displayln (cont-frac-rec (lambda (x) 1.0) euler-d 100))

; 1.39
(define (tan-cf-direct x k)
  (define x-square (square x))
  (define (iter k result)
    (if (= k 1) (/ x (- 1 result))
        (iter (- k 1)
              (/ x-square
                 (- (- (* 2 k) 1)
                    result)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x
                   (- (square x))))
             (lambda (i)
               (- (* 2 i) 1))
             k))

; 1.40
(define dx 0.0001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x)
            ((deriv f) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

; 1.41
(define (inc x) (+ x 1))

(define (double f)
  (lambda (x)
    (f (f x))))


; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43
(define (repeated f n)
  (define (iter k result)
    (if (= k 1)
        result
        (iter (- k 1)
              (compose f result))))
  (iter n f))

; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; 1.45
(define (average-damp f)
  (lambda (x) (average x f(x))))

(define (power x n)
  (if (= n 0) 1
      (* x (power x (- n 1)))))

(define (nth-root x n)
  (fixed-point ((repeated average (quotient n))
                (lambda (y) (/ x (power y (- n 1)))))
               1.0))

; 1.46
(define (iterative-improve good-enough? improve)
  (define (impl guess)
    (if (good-enough? guess)
        guess
        (impl (improve guess))))
  impl)

(define (sqrt x)
  ((iterative-improve
   (lambda (guess)
     (< (abs (- (square guess) x)) 0.001))
   (lambda (guess)
     (average guess (/ x guess))))
   1.0))

(define tolerance 0.00001)

(define (fixed-point-iter f first-guess)
  ((iterative-improve
    (lambda (x)
     (< (abs (x (f x))) tolerance))
    f)
   first-guess))