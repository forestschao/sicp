#lang racket
; Procedures and  Processes They Generate
; 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp b n)
  (define (iter b product n)
    (let ((nb (* b b))
          (nn (quotient n 2)))
      (cond ((= n 0) product)
            ((even? n) (iter nb product nn))
            (else (iter nb (* product b) nn)))))
  (iter b 1 n))

; 1.17 (correct to 1.18)
(define (double x)
  (+ x x))
(define (half x)
  (quotient x 2))

(define (multiple a b)
  (define (iter val result b)
    (let ((nval (double val))
          (nb (half b)))
      (cond ((= b 0) result)
            ((even? b) (iter nval result nb))
            (else (iter nval (+ result val) nb)))))
  (iter a 0 b))

; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)); p
                   (+ (* (+ p q) q) (* p q)); q
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.20
; Suppose it runs in the normal order
; (gcd 206 40)
; -> (gcd 40 (r 206 40))
; -> (gcd (r 206 40) (r 40 (r 206 40)))
; -> (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; -> (gcd ((r (r (1 + 2 + 1) 2)
; a b
; (gcd a b)
; (gcd b (a + b + 1))
; 206 40 0 0
;  40 6  0 1
;   6 4  1 2
;   4 2  2 4
;   2 0  4 7
;   0 0  7 11
; Therefore it is 18 times.
; The formula is:
; a0 = 0
; b0 = 0
; a(n) = b(n-1)
; b(n) = a(n) + b(n) + 1
; r(n) = a(n) + b(n)
; applicative is 4 times

; 1.21
(define (square x)
  (* x x))

(define (divisor? i n)
  (= (remainder n i) 0))

(define (smallest-divisor n)
  (define (try-it i n)
    (cond ((> (square i) n) n)
          ((divisor? i n) i)
          (else (try-it (+ i 1) n))))
  (try-it 2 n))
; 199   -> 199
; 1999  -> 1999
; 19999 -> 7

; 1.22
(define (prime? n)
  (= (smallest-divisor n) n))

(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (+ n 1)))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (search-iter n)
    (timed-prime-test n)
    (search-for-primes (+ n 2) end))
  (if (<= start end)
      (search-iter start)
      (newline)))

; 1.27
(define (expmod base exp n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) n)) n))
        (else
         (remainder (* base (expmod base (- exp 1) n)) n))))

(define (carmichael? n)
  (define (carmichael-iter i)
    (cond ((>= i n) #t)
          ((not (= (expmod i n n) i)) #f)
          (else (carmichael-iter (+ i 1)))))
  (carmichael-iter 1))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod-miller-robin base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ([square-val
                (expmod-miller-robin base
                                     (/ exp 2)
                                     m)])
               (if (and (= (remainder (square square-val) m) 1)
                        (= square-val 1)
                        (= square-val (- m 1)))
                   0
                   (remainder (square square-val) m))))
        (else
         (remainder (* base
                       (expmod-miller-robin base (- exp 1) m))
                     m))))

(define (miller-robin-test n)
  (define (try-it a)
    (= (expmod-miller-robin a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-robin-test n) (fast-prime? n (- times 1)))
        (else false)))
        