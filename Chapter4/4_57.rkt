#lang racket
(require "logic.rkt")
(require "database.rkt")

(define custom-rules '((assert! (rule (can-replace ?person-1 ?person-2)
                             (and (job ?person-2 ?job-2)
                                  (or (job ?person-1 ?job-2)
                                      (and (job ?person-1 ?job-1)
                                           (can-do-job ?job-1 ?job-2)))
                                  (not (same ?person-1 ?person-2)))))))

(define part-a '(can-replace ?x (Fect Cy D)))

(define part-b '(and (can-replace ?p1 ?p2)
                     (salary ?p1 ?s1)
                     (salary ?p2 ?s2)
                     (lisp-value < ?s1 ?s2)))

(define queries (list part-a part-b))

(define program (append assertions custom-rules queries))

(interpret program)