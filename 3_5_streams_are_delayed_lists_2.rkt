#lang racket
; 3.53
; 1 2 4 8 16....

; 3.54
;(define (cons-stream a b) (cons a (delay b)))
;(define (stream-car stream) (car stream))
;(define (stream-cdr stream) (force (cdr stream)))

(define stream-car stream-first) 
(define stream-cdr stream-rest)

;(define (delay b) (lambda () (b)))
;(define (force delay-object) (delay-object))

(define (integer-start-from n)
  (stream-cons n (integer-start-from (+ n 1))))

(define integers (integer-start-from 1))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (mul-streams s1 s2)
  (if (stream-empty? s1)
      empty-stream
      (stream-cons (* (stream-car s1)
                      (stream-car s2))
                   (mul-streams (stream-cdr s1)
                                (stream-cdr s2)))))
(define factorials
        (stream-cons 1
                     (mul-streams integers factorials)))

; 3.55
(define (add-streams s1 s2)
  (if (stream-empty? s1)
      empty-stream
      (stream-cons (+ (stream-car s1)
                      (stream-car s2))
                   (add-streams (stream-cdr s1)
                                (stream-cdr s2)))))
                      
(define (partial-sums s)
  (stream-cons (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))
; 3.56
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car) (stream-cons s1car
                                               (merge (stream-cdr s1)
                                                      s2)))
                 ((> s1car s2car) (stream-cons s2car
                                               (merge s1
                                                      (stream-cdr s2))))
                 (else (stream-cons s1car
                                    (merge (stream-cdr s1)
                                           (stream-cdr s2)))))))))
(define hammings (stream-cons 1
                       (merge (scale-stream hammings 2)
                              (merge (scale-stream hammings 3)
                                     (scale-stream hammings 5)))))

(define (show-stream s n)
  (if (= 0 n)
      (displayln "done.")
      (begin
        (displayln (stream-car s))
        (show-stream (stream-cdr s)
                     (- n 1)))))

; 3.57
; n starts from 0
; n = 0   if n = 0 or 1
;     n-1 otherwise
; The number of additions without memo-proc
; f(n) = f(n-1) + f(n-2) + 1

; 3.58
(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
            den
            radix)))

; 3.59
(define ones
  (stream-cons 1 ones))

(define (integrate-series s)
  (mul-streams s
               (stream-map (lambda (x) (/ 1 x))
                           integers)))
(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

; 3.60
(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s1) (stream-car s2))
                (mul-series s1 (stream-cdr s2)))))

(define square-of-sine-cosine
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))

; 3.61
(define (invert-unit-series s)
  (stream-cons 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (invert-unit-series s))))

; 3.62
(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "Divide by zero")
      (mul-series s1
                  (scale-stream (invert-unit-series s2)
                                (/ 1 (stream-car s2))))))
(define tan-series
  (div-series sine-series cosine-series))