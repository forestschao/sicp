#lang sicp
(#%require sicp-pict)

; 2.44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; 2.45
(define (split pos1 pos2)
  (define (split-painter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-painter painter (- n 1))))
          (pos1 painter (pos2 smaller smaller)))))
  split-painter)

(define right-split2 (split beside below))

; 2.46
(define (make-vect x y)
  (cons x y))

(define xcor-vec car)
(define ycor-vec cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vec v1) (xcor-vec v2))
             (+ (ycor-vec v1) (ycor-vec v2))))

(define (sub-vec v1 v2)
  (make-vect (- (xcor-vec v1) (xcor-vec v2))
             (- (ycor-vec v1) (ycor-vec v2))))

(define (scale-vect v factor)
  (make-vect (* (xcor-vec v) factor)
             (* (ycor-vec v) factor)))

; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame2 car)
(define edge1-frame2 cadr)
(define edge2-frame2 cddr)

; 2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; 2.49
(define (draw-line a b)
  (cons a b))
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(let ((tl (make-vect 0 1))
      (tr (make-vect 1 1))
      (bl (make-vect 0 0))
      (br (make-vect 0 1)))
  (segments->painter (list
                      (make-segment bl br)
                      (make-segment br tr)
                      (make-segment tr tl)
                      (make-segment tl bl)))
  (segments->painter (list
                      (make-segment bl tr)
                      (make-segment br tl))))
(let ((l (make-vect 0 0.5))
      (t (make-vect 0.5 1))
      (r (make-vect 1 0.5))
      (b (make-vect 0.5 0)))
  (segments->painter (list
                      (make-segment l t)
                      (make-segment t r)
                      (make-segment r b)
                      (make-segment b l))))

; 2.50
(define (flip-horiz2 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0) 
                     (make-vect 1.0 1.0)))

(define (rotate180-2 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; 2.51
(define (below-my painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

; 2.52
; skip a.
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside bl br)
           (beside tl tr))))

(define (square-limit2 painter n)
  (let ((combine4
         (square-of-four flip-horiz identity
                         rotate180 flip-vert)))
    (combine4 (corner-split painter n))))