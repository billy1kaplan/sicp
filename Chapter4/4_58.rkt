#lang racket
(require "logic.rkt")
(require "database.rkt")

(define custom-rules '((assert! (rule (big-shot ?person)
                                      (or (and (job ?person (?division . ?rest))
                                               (supervisor ?person ?boss)
                                               (not (job ?boss (?division . ?any))))
                                          (and (job ?person ?job)
                                               (not (supervisor ?person ?boss))))))))

(define query '(big-shot ?person))

(define queries (list query))

(define program (append assertions custom-rules queries))

(interpret program)