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

; 3.63
; Reasoner version is 1 + 2 + 3 + 4 + ... guess
(define (sqrt-improve guess x)
  (begin
    (display "guess: ")
    (displayln guess)
    (average guess (/ x guess))))
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-stream x)
  (define guess
  (stream-cons 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           guess)))
  guess)

; 3.64
(define (stream-limit s tolerance)
  (cond ((stream-empty? s) (error "empty-stream"))
        ((stream-empty? (stream-cdr s)) (stream-car s))
        (else
         (let ((s1 (stream-car s))
               (s2 (stream-car (stream-cdr s))))
           (if (< (abs (- s1 s2)) tolerance)
               s2
               (stream-limit (stream-cdr s) tolerance))))))

(define (sqrt-tolerate x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; 3.65
(define (log2-summands n)
  (stream-cons (/ 1 n)
               (stream-map - (log2-summands (+ n 1)))))

(define ln2
  (partial-sums (log2-summands 1)))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))
(define (accelerate-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

; 3.66
(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s t)
  (if (stream-empty? s)
      t
      (stream-cons (stream-car s)
                   (interleave t (stream-cdr s)))))

; 3.67
(define (pairs-all s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-all (stream-cdr s) t))))

; 3.68
; infinite loop
;(define (pairs-inf s t)
;  (interleave
;   (stream-map (lambda (x) (list (stream-car s) x))
;               t)
;   (pairs-inf (stream-cdr s) (stream-cdr t))))

; 3.69
(define (triples s t u)
  (stream-cons
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map (lambda (x) (list* (stream-car s) x))
                (stream-cdr (pairs t u)))

    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define pythagorean
  (stream-filter (lambda (x)
                   (let ((s1 (list-ref x 0))
                         (s2 (list-ref x 1))
                         (s3 (list-ref x 2)))
                     (= (+ (square s1) (square s2))
                        (square s3))))
                 (triples integers integers integers)))

; 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (stream-cons s1car
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (stream-cons s2car
                               (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define order-pair (weighted-pairs integers integers
                                   (lambda (x)
                                     (+ (list-ref x 0)
                                        (list-ref x 1)))))
(define no-235
  (stream-filter (lambda (x)
                   (and (> (remainder x 2) 0)
                        (> (remainder x 3) 0)
                        (> (remainder x 5) 0)))
                 integers))

(define stream-235
  (weighted-pairs no-235 no-235
                  (lambda (x)
                    (+ (* 2 (list-ref x 0))
                       (* 3 (list-ref x 1))
                       (* 5 (list-ref x 0) (list-ref x 1))))))

; 3.71
(define (cube x)
  (* x x x))

(define (cube-sum x)
  (+ (cube (list-ref x 0))
     (cube (list-ref x 1))))

(define cube-stream
  (weighted-pairs integers integers
                  cube-sum))

(define (reduce s weight)
  (let ((w1 (weight (stream-car s)))
        (w2 (weight (stream-car (stream-cdr s)))))
    (if (= w1 w2)
        (stream-cons w2 (reduce (stream-cdr (stream-cdr s)) weight))
        (reduce (stream-cdr s) weight))))

(define ramanujan
  (reduce cube-stream cube-sum))

; 3.72
(define (square-sum x)
  (+ (square (list-ref x 0))
     (square (list-ref x 1))))

(define (reduce-diff s weight num)
  (define (skip s num)
    (if (= num 1)
        s
        (skip (stream-cdr s) (- num 1))))
  (let ((w1 (weight (stream-car s)))
        (wn (weight (stream-car (skip s num)))))
    (if (= w1 wn)
        (stream-cons wn (reduce-diff (skip s (+ num 1))
                                     weight
                                     num))
        (reduce-diff (stream-cdr s) weight num))))

(define three-squares
  (reduce-diff (weighted-pairs integers integers square-sum)
               square-sum
               3))

; 3.73
(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (integral (scale-stream i (/ 1 C)) v0 dt)
     (scale-stream i R))))

; 3.74
; (define zero-crossings
;   (stream-map sign-change-detector sense-data
;                                    (stream-cons 0
;                                                 sense-data)))

; 3.75
; (define (make-zero-crossings input-stream last-value last-avpt)
;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;     (cons-stream (sign-change-detector avpt last-avpt)
;                  (make-zero-crossings (stream-cdr input-stream)
;                                       (stream-car input-stream)
;                                       avpt))))

; 3.76
;(define (smooth s)
;  (stream-map average s (stream-cdr s)))

; 3.77
(define (delay-integral delay-integrand initial-value dt)
  (stream-cons initial-value
               (let ((integrand (force delay-integrand)))
                 (if (stream-empty? integrand)
                     empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (solve f y0 dt)
  (define y (delay-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; (stream-ref (solve (lambda (y) y) 1 0.001) 1000)

; 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (delay-integral (delay dy) y0 dt))
  (define dy (delay-integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b)
                           (scale-stream dy a)))
  y)

; 3.79
(define (solve-2nd-generalize f y0 dy0 dt)
  (define y (delay-integral (delay dy) y0 dt))
  (define dy (delay-integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; 3.80
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (delay-integral (delay dvc) vc0 dt))
    (define il (delay-integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams (scale-stream il (- (/ R L)))
                             (scale-stream vc (/ 1 L))))
    (cons vc il)))

; 3.81
(define (stream-map-generalize f s1 s2)
  (stream-cons (f (stream-car s1) (stream-car s2))
               (stream-map-generalize f
                                      (stream-cdr s1)
                                      (stream-cdr s2))))

(define (rand-update x) 
  (modulo (+ 101 (* x 713)) 53)) 

(define random-init (rand-update 10))

 (define (rand requests)
   (define (process request last)
     (if (eq? request "generate")
         (rand-update last)
         (cdr request)))
   (define rand-result
     (stream-cons random-init
                  (stream-map-generalize process requests rand-result)))
   (stream-cdr rand-result))

(define test-requests
  (stream-cons (cons "reset" 5)
    (stream-cons "generate"
      (stream-cons "generate"
        (stream-cons (cons "reset" 5)
          (stream-cons "generate"
            (stream-cons "generate"
              (stream-cons (cons "reset" 5)
                (stream-cons "generate"
                  empty-stream)))))))))

; 3.82
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

; (define (estimate-integral P x1 x2 y1 y2)
;   (scale-stream (monte-carlo (stream-map P rand-points))
;                 (* (- x2 x1) (- y2 y1))))

