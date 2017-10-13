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

; New stuff
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment v)
  (car v))

(define (end-segment v)
  (cdr v))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(define segment (make-segment v1 v2))
(start-segment segment)
(end-segment segment)
