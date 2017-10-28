#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((eq? x (car set)) set)
        ((> (car set) x) (append (list x) set))
        (else (append (list (car set)) (adjoin-set x (cdr set))))))

(define (adjoin-set1 x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> (car set) x) (cons x set))
        (else (cons (car set) (adjoin-set1 x (cdr set))))))

(adjoin-set1 3 (list 2 4 5))
