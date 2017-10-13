#lang sicp
(#%require sicp-pict)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect v1 v2)
  (make-vect
    ( +
      (xcor-vect v1)
      (xcor-vect v2))
    ( +
      (ycor-vect v1)
      (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
    ( -
      (xcor-vect v1)
      (xcor-vect v2))
    ( -
      (ycor-vect v1)
      (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect
    ( * (xcor-vect v) s)
    ( * (ycor-vect v) s)))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment v)
  (car v))

(define (end-segment v)
  (cdr v))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))

(define bl (make-vect 0 0))
(define tl (make-vect 0 1))
(define tr (make-vect 1 1))
(define br (make-vect 1 0))

(define (outliner frame)
  (segments->painter (list (make-segment bl br)
                           (make-segment bl tl)
                           (make-segment tl tr)
                           (make-segment tr br))))

(define (x-frame frame)
  (segments->painter (list (make-segment bl br)
                           (make-segment bl tl)
                           (make-segment tl tr)
                           (make-segment tr br)
                           (make-segment bl tr)
                           (make-segment br tl)
                           )))

(define midtop (make-vect 0.5 1))
(define midbot (make-vect 0.5 0))
(define midleft (make-vect 0 0.5))
(define midright (make-vect 1 0.5))

(define (diamond-frame frame)
  (segments->painter (list (make-segment bl br)
                           (make-segment bl tl)
                           (make-segment tl tr)
                           (make-segment tr br)
                           (make-segment midleft midtop)
                           (make-segment midtop midright)
                           (make-segment midbot midright)
                           (make-segment midleft midbot)
                           )))

(define top-left-eye (make-vect 0.3 0.8))
(define bot-left-eye (make-vect 0.3 0.6))

(define top-right-eye (make-vect 0.7 0.8))
(define bot-right-eye (make-vect 0.7 0.6))

(define smile-left (make-vect 0.2 0.4))
(define smile-center-left (make-vect 0.3 0.3))
(define smile-center (make-vect 0.5 0.27))
(define smile-center-right (make-vect 0.7 0.3))
(define smile-right (make-vect 0.8 0.4))
(define (smiley-painter frame)
  (segments->painter (list (make-segment bot-left-eye top-left-eye)
                           (make-segment bot-right-eye top-right-eye)
                           (make-segment smile-left smile-center-left)
                           (make-segment smile-center-left smile-center)
                           (make-segment smile-center smile-center-right)
                           (make-segment smile-center-right smile-right))))

(define simple-frame (make-frame (make-vect 0 0) (make-vect 4 1) (make-vect 1 4))) 
(paint (outliner simple-frame))
(newline)
(newline)
(paint (x-frame simple-frame))
(newline)
(newline)
(paint (diamond-frame simple-frame))
(newline)
(paint (smiley-painter simple-frame))
