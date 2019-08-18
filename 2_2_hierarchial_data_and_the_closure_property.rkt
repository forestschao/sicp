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
; (car (cdaddr (list 1 3 (list 5 7) 9)))

; (caar (list (list 7)))
 
(define long-list (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (cadadr (cadadr (cadadr long-list)))

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

; 2.30
(define (square-tree-rec tree)
  (cond ((null? tree) empty)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-rec (car tree))
                    (square-tree-rec (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-map subtree)
             (square subtree)))
       tree))

(define tree-a
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

; 2.31
(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map f subtree)
             (f subtree)))
       tree))

; 2.32
; empty -> (())
; first, rest = all the subsets without the first
;             + all the subsets with the first
;             = (subsets (cdr s)) + (first + (subsets (cdr s)))
(define (subsets s)
  (if (null? s)
      (list empty)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))

; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-acc p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              empty
              sequence))

(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-acc sequence)
  (accumulate (lambda (x y)
                (+ y 1))
              0
              sequence))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coefficient higher-terms)
                (+ this-coefficient
                   (* higher-terms x)))
              0
              coefficient-sequence))

; 2.35
(define (count-leaves-acc t)
  (accumulate +
              0
              (map (lambda (subtree)
                     (cond ((null? subtree) 0)
                           ((not (pair? subtree)) 1)
                           (else (count-leaves-acc subtree))))
                   t)))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37
(define v (list 1 2 3 4))
(define m (list
           (list 1 2 3 4)
           (list 4 5 6 6)
           (list 6 7 8 9)))
(define n (list
           (list 1 10)
           (list 2 20)
           (list 3 30)
           (list 4 40)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons empty mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

; (fold-right / 1 (list 1 2 3)) -> 3/2
; (fold-left / 1 (list 1 2 3))  -> 1/6
; (fold-right list empty (list 1 2 3)) -> (1 (2 (3 ()))
; (fold-left list empty (list 1 2 3))  -> (((() 1) 2) 3)

; if (op a b) == (op b a)
;   fold-right == fold-left

(define (reverse-fold-right sequence)
  (fold-right (lambda (x result)
                (append result (list x)))
              empty
              sequence))

(define (reverse-fold-left sequence)
  (fold-left (lambda (result y)
               (cons y result))
             empty
             sequence))

; 2.40
(define (flatmap proc seq)
  (accumulate append empty (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      empty
      (cons low
            (enumerate-interval (+ 1 low)
                                high))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (make-pair-sum p)
  (list (car p) (cadr p) (+ (car p) (cadr p))))

(define (prime-sum? p)
  (prime? (+ (car p) (cadr p))))

(define (prime? n)
  (define (iter i)
    (cond ((> (square i) n) true)
          ((= (remainder n i) 0) false)
          (else (iter (+ i 1)))))
  (iter 2))
        
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; 2.41
(define (distinct-sum-triple n s)
  (filter (lambda (p)
            (= (accumulate + 0 p) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
                       
; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens-lr board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define empty-board empty)
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (let ((head (car positions)))
    (define (iter i rest)
      (if (null? rest)
           true
           (let ((c (car rest)))
             (if (or (= head c)
                     (= (+ head i) c)
                     (= (- head i) c))
                 false
                 (iter (+ i 1) (cdr rest))))))
    (iter 1 (cdr positions))))


; 2.43
; The complexity of Louis Reasoner is
; T(k) = T(k-1) * b + size(T(k-1))*b
; T(k) = b^k
; While the complexity of the origin code is:
; T(k) = T(k-1) + size(T(k-1)) * b
; Therefore:
; If the original code spends T time,
; Then the new one spends 8^8 * T.



