#lang racket

(require "compiler.rkt")

(define mystery '(define f (lambda (x)
                             (+ x (g (+ x 2))))))
(set-label-start! 14) ;; just to match the book
(statements (compile mystery 'val 'next))