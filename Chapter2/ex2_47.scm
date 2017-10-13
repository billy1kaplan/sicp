#lang sicp

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-cons frame)
  (car frame))

(define (edge1-frame-cons frame)
  (cadr frame))

(define (edge2-frame-cons frame)
  (cdr (cdr frame)))

(define frame1 (make-frame-cons 0 1 2))

((lambda(x) x) frame1)
(origin-frame-cons frame1)
(edge1-frame-cons frame1)
(edge2-frame-cons frame1)

