#lang sicp

; 3.1
(define (make-accumulator acc)
  (lambda (delta)
    (set! acc (+ acc delta))
    acc))

;(define A (make-accumulator 5))
;(A 10)
;(A 10)

; 3.2
(define (make-monitored proc)
  (define count 0)
  (define (mf m)
    (cond ((eq? m 'how-many-calls?) count)
          ((eq? m 'reset-count) (set! count 0) count)
          (else (set! count (+ count 1)) (proc m))))
  mf)

(define s (make-monitored sqrt))

;(s 100)
;(s 'how-many-calls?)
;(s 10)
;(s 'how-many-calls?)
;(s 'reset-count)
;(s 9)
;(s 'how-many-calls?)

; 3.3 & 3.4
(define (make-account balance password)
  (define wrong-password-count 0)
  
  (define (withdraw amount)
    (clear-wrong-password-count)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (clear-wrong-password-count)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch p m)
    (cond ((not (eq? p password)) reject)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  
  (define (reject . x)
    (set! wrong-password-count (+ 1 wrong-password-count))
    (if (<= wrong-password-count 7)
        "Incorrect password"
        (call-the-cops)))
  
  (define (call-the-cops)
    "CALL THE COPS.")

  (define (clear-wrong-password-count)
    (set! wrong-password-count 0))
  
  dispatch)

(define (bank-account-test)
  (define acc (make-account 100 'secret-password))
  ((acc 'secret-password 'withdraw) 40)
  ((acc 'some-other-password 'deposit) 50)
  ((acc '2 'deposit) 50)
  ((acc '3 'deposit) 50)
  ((acc '4 'deposit) 50)
  ((acc '5 'deposit) 50)
  ((acc '6 'deposit) 50)
  ((acc '7 'deposit) 50)
  ((acc 'secret-password 'deposit) 50)
  ((acc '8 'deposit) 50)
  )

; 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (* (- x2 x1)
     (- y2 y1)
     (monte-carlo trials
                  (lambda ()
                    (predicate (random-in-range x1 x2)
                               (random-in-range y1 y2))))))

(define (square x) (* x x))
(define (in-unit-circle x y)
  (<= (+ (square x)
         (square y))
      1))

; 3.6
(define random-init 37)
(define (rand-update x)
  (+ x 1))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
            ((eq? m 'reset) (lambda (init) (set! x init) x))
            (else "Unsupported operation -- RAND" m)))))

  
