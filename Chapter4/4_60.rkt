#lang racket
(require "logic.rkt")
(require "database.rkt")

;; This happens because lives-near is a symmetric property. If A lives near B, then B lives near A.
;; Our rule finds both of these cases

;; The duplication can be removed as follows:

(define (compare-names name1 name2)
  (define (create-name name)
    (foldr (lambda (a b) (string-append (symbol->string a) b)) "" name))
  (string<? (create-name name1) (create-name name2)))

;; Arbitrarily pick the person with the name that comes earlier in the alphabet to remove duplicates
(define query '(and (lives-near ?person-1 ?person-2)
                    (lisp-value (lambda (name1 name2)
                                       (string<? (foldr (lambda (a b) (string-append (symbol->string a) b)) "" name1)
                                                 (foldr (lambda (a b) (string-append (symbol->string a) b)) "" name2)))
                                     ?person-1
                                     ?person-2)))

(define program (append assertions (list query)))

(interpret program)