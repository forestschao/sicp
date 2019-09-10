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
          ((eq? m 'join) (cons withdraw deposit))
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

; 3.7
(define (make-joint account password own-password)
  (let ((joint (account password 'join)))
    (if (not (pair? joint))
        joint
        (lambda (p m)
          (cond ((not (eq? p own-password)) "Incorrect password.")
                ((eq? m 'withdraw) (car joint))
                ((eq? m 'deposit) (cdr joint))
                ((eq? m 'join) joint)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))))))

(define (install-join-account-test)
  (define peter-acc (make-account 100 'peter))
  (define paul-acc (make-joint peter-acc 'peter 'paul))

  ((peter-acc 'peter 'withdraw) 20)
  ((paul-acc 'paul 'deposit) 10)
  'done)

; 3.8
(define f 
  (let ((is-first true)) 
    (lambda (x)
      (if is-first
          (begin
            (set! is-first false)
            x)
          0))))

; (+ (f 0) (f 1))

; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; (define v (list 'a 'b 'c 'd))
; (define w (mystery v))

; 3.16
(define (list-count)
  (define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))
  (define l1 (list 1 2 3))
  (count-pairs l1)

  (define t1 (list 1 2))
  (define l2 (cons (cdr t1) t1))
  (count-pairs l2)

  (define t71 (list 1))
  (define t72 (cons t71 t71))
  (define l3 (cons t72 t72))
  (count-pairs l3)
  'done)

; 3.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (append! l t)
  (set-cdr! (last-pair l) (cons t '())))

(define (count-pairs x)
  (let ((visited '()))
    (define (explore x)
      (cond ((or (not (pair? x))
                 (memq x visited)) 0)
            (else
             (set! visited (cons x visited))
             (+ (explore (car x))
                (explore (cdr x))
                1))))
    (explore x)))

(define (count-pairs-test)
  (define l1 (list 1 2 3))
  (count-pairs l1)

  (define t1 (list 1 2))
  (define l2 (cons (cdr t1) t1))
  (count-pairs l2)

  (define t71 (list 1))
  (define t72 (cons t71 t71))
  (define l3 (cons t72 t72))
  (count-pairs l3)
  'done)

; 3.18
(define (exist-cycle? x)
  (let ((visited '()))
    (define (check x)
      (cond ((null? x) false)
            ((memq (car x) visited) true)
            (else
             (set! visited (cons (car x) visited))
             (check (cdr x)))))
    (check x)))

(define (exist-cycle-test)
  (define a (make-cycle (list '(1 2 3))))
  (exist-cycle? a)
  (define b (list '(1 2 3)))
  (exist-cycle? b)
  'done)

; 3.19
(define (is-cycle? x)
  (define (iter slow fast)
    (cond ((or (null? fast)
               (null? (cdr fast))) false)
          ((eq? slow fast) true)
          (iter (cdr slow)
                (cddr fast))))
  (iter x x))

; 3.21
(define (make-queue) (cons '() '()))
(define (empty-queue? q) (null? (front-ptr q)))
(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (print queue)
  (define (iter q)
    (cond ((null? q)
           (display ")")
           (newline))
          (else
           (display (car q))
           (if (null? (cdr q))
               (display "")
               (display " "))
           (iter (cdr q)))))
  (display "(")
  (iter (front-ptr queue)))

(define (install-print-queue)
  (define q1 (make-queue))
  (print q1)
  (insert-queue! q1 'a)
  (print q1)
  (insert-queue! q1 'b)
  (print q1)
  (delete-queue! q1)
  (print q1)
  (delete-queue! q1)
  (print q1)
  'done)

(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (display front-ptr)
               (newline)
               (display rear-ptr)
               (newline)
               (newline))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               (display front-ptr)
               (newline)
               (display rear-ptr)
               (newline)
               (newline))
              )))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (disptach m . v)
      (cond ((eq? m 'front-queue) (front-queue))
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'insert-queue!) (insert-queue! (car v)))
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "UNDEFINED operations"))))
    disptach))

(define (install-deque)
  (define (make-deque) (cons '() '()))
  (define (empty-deque? q) (null? (front-ptr q)))
  (define (front-deque q)
    (if (empty-deque? q)
        (error "FRONT called with an empty deque")
        (car (front-ptr q))))
  (define (rear-deque q)
    (if (empty-deque? q)
        (error "REAR called with an empty deque")
        (car (rear-ptr q))))
  (define (link node) (cdr node))
  (define (front-insert-deque! deque item)
    (let ((new-pair (cons item (cons '() '())))
          (head (front-ptr deque)))
      (cond ((empty-deque? deque)
             (set-cdr! deque new-pair))
            (else
             (set-cdr! (link new-pair) head)
             (set-car! (link head) new-pair)))
      (set-car! deque new-pair)
      deque))

  (define (rear-insert-deque! deque item)
    (let ((new-pair (cons item (cons '() '())))
          (tail (rear-ptr deque)))
      (cond ((empty-deque? deque)
             (set-car! deque new-pair))
            (else
             (set-car! (link new-pair) tail)
             (set-cdr! (link tail) new-pair)))
      (set-cdr! deque new-pair)
      deque))

  (define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "FRONT DELETE on an empty deque")) 
          (else
           (let ((head (front-ptr deque)))
             (let ((second (cdr (link head))))
               (cond ((null? second)
                      (set-car! deque '())
                      (set-cdr! deque '()))
                     (else
                      (set-car! (link second) '())
                      (set-car! deque second)))
               deque)))))

  (define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "REAR DELETE on an empty deque")) 
          (else
           (let ((tail (rear-ptr deque)))
             (let ((second (car (link tail))))
               (cond ((null? second)
                      (set-car! deque '())
                      (set-cdr! deque '()))
                     (else
                      (set-cdr! (link second) '())
                      (set-cdr! deque second)))
               deque)))))

  (define (print-deque q)
    (define (iter q)
      (cond ((null? q)
             (display ")")
             (newline))
            (else
             (display (car q))
             (if (null? (cdr (link q)))
                 (display "")
                 (display " "))
             (iter (cdr (link q))))))
    (display "(")
    (iter (front-ptr q))) 

  (define dq (make-deque))
  (empty-deque? dq)
  (print-deque (front-insert-deque! dq 1))
  (print-deque (front-insert-deque! dq 2))
  (print-deque (rear-insert-deque! dq 3))
  (print-deque (front-delete-deque! dq))
  (print-deque (rear-delete-deque! dq))
  )


(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (make-table-multiple)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup key . keys)
      (define (iter table key keys)
        (let ((subtable (assoc key (cdr table))))
          (if subtable
              (if (null? keys)
                  (cdr subtable)
                  (iter subtable (car keys) (cdr keys)))
              false)))
      (iter local-table key keys))
    
    (define (insert! value key . keys)
      (define (create-list value keys)
        (if (null? (cdr keys))
            (cons (car keys) value)
            (list (car keys)
                  (create-list value (cdr keys)))))
      (define (iter table value key keys)
        (let ((subtable (assoc key (cdr table))))
          (if subtable
              (if (null? keys)
                  (set-cdr! subtable value)
                  (iter subtable value (car keys) (cdr keys)))
              (set-cdr! table
                        (cons (create-list value (cons key keys))
                              (cdr table))))))
      (iter local-table value key keys))
        
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (make-table-multiple-test)
  (define mt (make-table-multiple))
  (define get (mt 'lookup-proc))
  (define put (mt 'insert-proc!))
  (put 3 'a 'b 'c)
  (get 'a 'b 'c))

(define (make-table-binary)
  (let ((local-table '()))
    ; (key value) left right
    (define (make-node key value)
      (list (cons key value) '() '()))
    (define (entry node) (caar node))
    (define (value node) (cdar node))
    (define (left node) (cadr node))
    (define (right node) (caddr node))
    (define (set-value! node) (set-cdr! (car node)))
    (define (set-left! node new-left)
      (set-car! (cdr node) new-left))
    (define (set-right! node new-right)
      (set-car! (cddr node) new-right))
    
    (define (check key node)
      (cond ((null? node) false)
            ((= key (entry node)) (value node))
            ((< key (entry node)) (check key (left node)))
            ((> key (entry node)) (check key (right node)))))
    (define (lookup key) (check key local-table))

    (define (add-node key value node)
      (if (< (entry node) key)
          (if (null? (right node))
              (set-right! node (make-node key value))
              (add-node key value (right node)))
          (if (null? (left node))
              (set-left! node (make-node key value))
              (add-node key value (left node)))))

    (define (insert! key value)
      (if (null? local-table)
          (set! local-table (make-node key value))
          (let ((node (check key local-table)))
            (cond (node (set-value! node value))
                  (
                   (add-node key value local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (install-make-table-binary-test)
  (define bt (make-table-binary))
  (define (put key value) ((bt 'insert-proc!) key value))
  (define (get key) ((bt 'lookup-proc) key))


  (put 2 'b)
  (put 1 'a)
  (put 3 'c)
  (get 4))

(define (install-fib-test)
  (define (fib n)
    (display "fib ")
    (display n)
    (newline)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))
  (define (memoize f)
    (let ((table (make-table-binary)))
      (lambda (x)
        (let ((prev-computed ((table 'lookup-proc) x)))
          (or prev-computed
              (let ((result (f x)))
                ((table 'insert-proc!) x result)
                result))))))

  (define memo-fib (memoize fib))
  (memo-fib 4)
  (memo-fib 4)
  )

; 3.28
(define (inverter input output)
  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)


(define (and-gate a1 a2 output)
  
  (define (logical-and a1 a2)
    (cond ((and (= a1 1) (= a2 1)) 1)
          (else 0)))
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate o1 o2 output)
  (define (logical-or o1 o2)
    (cond ((or (= 1 o1) (= 1 o2)) 1)
          (else 0)))
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

; 3.29
; delay-time = 2 * inverter-delay + and-delay
(define (or-gate-comp o1 o2 output)
  (let ((inv-o1 (make-wire))
        (inv-o2 (make-wire))
        (and-1 (make-wire)))
    (inverter o1 inv-o1)
    (inverter o2 inv-o2)
    (and-gate inv-o1 inv-o2 and-1)
    (inverter and-1 output)
    'ok))

; 3.30
; max(and + invert, or) + and
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; 2 half + or
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; n * full
; 2n * half + n * or
; 2n * max(and + invert, or) + 2n * and + n * or
(define (ripple-carry A B S c-in)
  (define (iter a-remains b-remains s-remains c-in)
    (cond ((null? a-remains) 'ok)
          (else
           (let ((c-temp (make-wire)))
             (full-adder (car a-remains)
                         (car b-remains)
                         (car s-remains)
                         (c-temp))
             (iter (cdr a-remains)
                   (cdr b-remains)
                   (cdr s-remains)
                   c-temp)))))
  (iter A B S c-in))

; 3.31
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))
        
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))
                   
  
