#lang racket
(require "logic.rkt")
(require "database.rkt")

(define part-a '(and (supervisor ?person (Bitdiddle Ben))
                     (address ?person ?a)))
(define part-b '(and (salary (Bitdiddle Ben) ?ben-salary)
                     (salary ?person ?amount)
                     (lisp-value < ?amount ?ben-salary)))
(define part-c '(and (supervisor ?p ?s)
                     (not (job ?s (computer . ?a)))
                     (job ?s ?j)))

(define queries (list part-a part-b part-c))

(define program (append assertions
                        queries))

(interpret program)