#lang sicp

(define (element-of-set? el set)
  (cond ((null? set) #f)
        ((equal? el (car set)) #t)
        (else (element-of-set? el (cdr set)))))

(define (adjoin-set el set)
  (cons el set))

(define (union-set set1 set2)
  (if (null? set1)
    set2
    (cons (car set1) (union-set (cdr set1) set2))))
; more concise (append set1 set2)

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define set1 (list 1 2 3))
(define set2 (list 3 4 5))
(element-of-set? 3 set2)
(adjoin-set 4 set2)
(union-set set1 set2)
(intersection-set set1 set2)
