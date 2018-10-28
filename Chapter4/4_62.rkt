#lang racket
(require "logic.rkt")
(require "database.rkt")

(define custom-assertions '((assert! (rule (last-pair (?x) (?x))))
                            (assert! (rule (last-pair (?x . ?y) (?l))
                                           (last-pair ?y (?l))))))

(define queries '((last-pair (3) ?x) ; ?x = (3)
                  (last-pair (1 2 4 3) ?x) ; ?x = (3)
                  (last-pair (2 ?x) (3)))) ; ?x = (3)
                  ;(last-pair ?x (3)))) ; ?x = (... 3) <- Infinite number of answers

(define program (append assertions custom-assertions queries))

(interpret program)