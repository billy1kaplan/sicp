#lang racket

(require "compiler.rkt")

(define factorial-alt '(define factorial (lambda (n)
                                           (if (= n 1)
                                               1
                                               (* n (factorial (- n 1)))))))
(define comp-factorial (statements (compile factorial-alt 'val 'next)))
(reset-compiler)

(define factorial '(define factorial (lambda (n)
                                       (if (= n 1)
                                           1
                                           (* (factorial (- n 1)) n)))))
(define original-factorial (statements (compile factorial 'val 'next)))

(define (compare list1 list2)
  (define (iter list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          ((equal? (car list1) (car list2))
           (iter (cdr list1) (cdr list2)))
          (else
           (display "ONE: ")
           (newline)
           (display list1)
           (newline)
           (display "TWO: ")
           (newline)
           (display list2))))
  (iter list1 list2))

(define (diffs diff-list)
  (if (null? diff-list)
      (newline)
      (begin
        (display "DIFF: ")
        (newline)
        (display (car (car diff-list)))
        (newline)
        (display (cdr (car diff-list)))
        (newline)
        (diffs (cdr diff-list)))))
    
(compare comp-factorial original-factorial)

;; It seems like the alternative requires us to save the environment in order to perform the recursive call (factorial-alt (- n 1))
;; while this is unnececessary for a symbol lookup on n. However, the other performs a similar save on the argl.
;; Perhaps this makes the original a bit faster if saving environment is slower than saving the argument list?