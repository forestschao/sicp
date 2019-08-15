#lang racket
; 2.17
(define (last-pair l)
  (cond ((null? l) (error "Empty list has no last elemnt -- LIST"))
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))))

; 2.18
(define (reverse l)
  (define (iter remain result)
    (if (null? remain)
        result
        (iter (cdr remain) (cons (car remain) result))))
  (iter l empty))

; 2.19
(define no-more? empty?)
(define except-first-denomination cdr)
(define first-denomination car)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; 2.20
(define (same-parity f . r)
  (define (iter r)
    (if (null? r)
        empty 
        (let ((rest (iter (cdr r))))
          (if (= (remainder f 2) (remainder (car r) 2))
              (cons (car r) rest)
              rest))))
  (iter (cons f r)))

; 2.21
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
      empty
      (cons (square (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

; 2.22
; This lead to the list
; ((() a) b) ... )

; 2.23
(define (for-each f items)
  (if (null? items)
      true
      (begin
        (f (car items))
        (for-each f (cdr items)))))

; 2.24
; (1 (2 (3 4)))

; 2.25
(car (cdaddr (list 1 3 (list 5 7) 9)))

(caar (list (list 7)))
 
(define long-list (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadadr (cadadr (cadadr long-list)))

; 2.26
; (append x y): (1 2 3 4 5 6
; (cons x y): ((1 2 3) 4 5 6)
; (list x y): ((1 2 3) (4 5 6))

; 2.27
(define (deep-reverse l)
  (define (iter remain result)
    (if (null? remain)
        result
        (iter (cdr remain)
              (cons (deep-reverse (car remain))
                    result))))
  (if (pair? l)
      (iter l empty)
      l))

; 2.28
(define (fringe l)
  (if (null? l)
      empty
      (let ((first (car l)))
        (if (pair? first)
            (append (fringe first)
                    (fringe (cdr l)))
            (cons first (fringe (cdr l)))))))

(define (fringe-tree l)
  (define (iter t result)
    (if (null? t)
        result
        (let ((first (car t)))
          (if (not (pair? first))
              (cons first result)
              (iter first (fringe-tree (cdr l)))))))
  (iter l empty))
          
(define x (list (list 1 2) (list 3 4)))

; 2.29
(define (make-mobile left right) 
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

(define (total-weight m)
  (if (not (pair? m))
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

(define (balanced? m)
  (if (number? m)
      true
      (let ((left (left-branch m))
            (right (right-branch m))
            (torque (lambda (b)
                      (* (total-weight (branch-structure b))
                         (branch-length b)))))
        (and (= (torque left) (torque right))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))))

(define mobile-a (make-mobile (make-branch 2 3) (make-branch 2 3)))

  
 (define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8)))))

 (define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4))))) 
      