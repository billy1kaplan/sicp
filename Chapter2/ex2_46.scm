#lang sicp

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

(let ((v1 (make-vect 3 4))
      (v2 (make-vect 5 7)))
  ;(add-vect v1 v2))
  ;(sub-vect v2 v1))
  (scale-vect v1 3))