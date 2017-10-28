#lang sicp

(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((or (null? list1) (null? list2)) #f)
        ((not (eq? (car list1) (car list2))) #f)
        (else (equal? (cdr list1) (cdr list2)))))

(equal? '(this is a list)
        '(this is a list))

(equal? '(this is a list)
        '(this (is a) list))
