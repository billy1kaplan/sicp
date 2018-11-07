#lang racket

(require "compiler.rkt")

(define factorial '(begin
                     (define factorial (lambda (n)
                                         (if (= n 1)
                                           1
                                           (* n (factorial (- n 1))))))))
  (define rec-fact (statements (compile factorial 'val 'next)))
(reset-compiler)

(define iter-factorial '(begin
                     (define iter (lambda (product counter n)
                                    (if (> counter n)
                                        product
                                        (iter (* counter product)
                                              (+ counter 1)))))
                     (define factorial (lambda (n)
                                         (iter 1 1)))))
(define iter-fact (statements (compile iter-factorial 'val 'next)))