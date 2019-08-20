#lang sicp
; 2.53
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else
         (memq item (cdr x)))))

; 2.54
(define (my-equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else false)))

; 2.55
; transform into (quote (quote abracadabra))
; (cdr ''abracadabra)

; 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((in-sum? exp)
         (make-in-sum (deriv (in-addend exp) var)
                      (deriv (in-augend exp) var)))
        ((in-product? exp)
         (make-in-sum (make-in-product (in-multiplier exp)
                                       (deriv (in-multiplicand exp) var))
                      (make-in-product (deriv (in-multiplier exp) var)
                                       (in-multiplicand exp))))
        ((exponentiation? exp)
         (let ((e (exponent exp))
               (b (base exp)))
           (cond ((=number? e 0) 0)
                 ((=number? e 1) (deriv b var))
                 (else
                  (make-product (make-product e
                                              (make-exponentiation b (- e 1)))
                                (deriv b var))))))
       ; ((singleton? exp) (deriv (car exp) var))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define addend cadr)
(define (augend exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cons '+ (cddr exp))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define multiplier cadr)
(define (multiplicand exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cons '* (cddr exp))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else
         (list '** b e))))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (car exp) '**)))

(define base cadr)
(define exponent caddr)

; 2.57
(define (singleton? exp)
  (and (pair? exp)
       (null? (cdr exp))))

(define (simplify exp)
  (if (singleton? exp)
      (car exp)
      exp))

(define (extract-before item x)
  (define (iter rest result)
    (cond ((null? rest) false)
          ((eq? item (car rest))
           (simplify (reverse result)))
          (else
           (iter (cdr rest) (cons (car rest) result)))))
  (iter x '()))

(define (extract-after item x)
  (simplify (cdr (memq item x))))

(define (make-in-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (in-sum? x)
  (and (pair? x)
       (memq '+ x)))

(define (in-addend exp)
  (extract-before '+ exp))

(define (in-augend exp)
  (extract-after '+ exp))


(define (make-in-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (in-product? x)
  (and (pair? x)
       (memq '* x)))

(define (in-multiplier exp)
  (extract-before '* exp))

(define (in-multiplicand exp)
  (extract-after '* exp))

; 2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((element-of-set? (car set2) set1)
         (union-set set1 (cdr set2)))
        (else
         (union-set (adjoin-set (car set2) set1)
                    (cdr set2)))))

; 2.60
(define (element-of-set-dup? x set1)
  (cond ((null? set1) false)
        ((equal? x (car set1)) true)
        (else
         (element-of-set-dup? x (cdr set1)))))

(define (adjoin-set-dup x set)
  (cons x set))

(define intersection-set-dup intersection-set)
(define (union-set-dup set1 set2)
  (append set1 set2))

; Only when there are a lot of adjoin set and union set
; and few other operations.

; 2.61
(define (element-of-set-order? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else
         (element-of-set? x (cdr set)))))

(define (intersection-set-order set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-order (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set-order (cdr set1) set2))
              ((> x1 x2)
               (intersection-set-order set1 (cdr set2)))))))

(define (adjoin-set-order x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set-order x (cdr set))))))

; 2.62
(define (union-set-order set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set-order (cdr set1)
                                                      (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set-order (cdr set1) set2)))
                 ((> x1 x2) (cons x2 (union-set-order set1 (cdr set2)))))))))

; 2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define tree1
  (make-tree
   7
   (make-tree
    3
    (make-tree 1 '() '())
    (make-tree 5 '() '()))
   (make-tree
    9
    '()
    (make-tree 11 '() '()))))

(define tree2
  (make-tree
   3
   (make-tree 1 '() '())
   (make-tree
    7
    (make-tree 5 '() '())
    (make-tree 9
               '()
               (make-tree 11 '() '())))))
; They do not differ.
; nope.
; tree->list-1: T(l) = T(l/2) + T(l/2) + 1 + l/2: n log n
; tree->list-2: T(l) = T(l/2) + T(l/2) + 1: n

; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remain-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remain-elts))))))))
;         5
;       /   \
;      1     9
;       \   / \
;        3 7  11
; The order is O(n)

; 2.65
(define (union-set-tree set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (list->tree
          (union-set-order
           (tree->list-2 set1)
           (tree->list-2 set2))))))

(define (intersection-set-tree set1 set2)
  (cond ((null? set1) '())
        ((null? set2) '())
        (else
         (list->tree
          (intersection-set-order
           (tree->list-2 set1)
           (tree->list-2 set2))))))

; 2.66
(define key car)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

; 2.67
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define symbol-leaf cadr)
(define weight-leaf caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define left-branch2 car)
(define right-branch2 cadr)
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch2 branch))
        ((= bit 1) (right-branch2 branch))
        (else (error "bad bit -- CHOOSE BRANCH" bit))))

(define (adjoin-set2 x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set2 x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set2 (make-leaf (car pair)
                                (cadr pair))
                     (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

;         .
;       /   \
;      A     .
;           / \
;          B   .
;             / \
;             D  C

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; ADABBCA

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set2? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set2? x (cdr set)))))
      
(define (encode-symbol symbol tree)
  (define (encode-1 result current-branch) 
    (cond ((leaf? current-branch) (reverse result))
          ((element-of-set2? symbol (symbols (left-branch2 current-branch)))
           (encode-1 (cons 0 result) (left-branch2 current-branch)))
          ((element-of-set2? symbol (symbols (right-branch2 current-branch)))
           (encode-1 (cons 1 result) (right-branch2 current-branch)))
          (else
           (error "Symbol is not found -- ENCODE" symbol))))
  (encode-1 '() tree))

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge nodes)
  (if (= (length nodes) 1)
      (car nodes) 
      (successive-merge
       (adjoin-set2 (make-code-tree (car nodes)
                                    (cadr nodes))
                    (cddr nodes)))))

(define constructed-tree (generate-huffman-tree '((B 2) (C 1) (D 1) (A 4))))

; 2.70
(define lyrics-tree
  (generate-huffman-tree
   '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define lyrics-messages
  (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
          lyrics-tree))
; (length lyrics-messages): 84

; 2.71
; bits to encode the most frequent: 1
; bits to encode the least frequence:
;       1; (n = 1)
;   n - 1; (n > 2)

; 2.72
; encode most: 2 O(1)
; encode least: O(n^2)