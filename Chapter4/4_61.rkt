#lang racket
(require "logic.rkt")
(require "database.rkt")

(define custom-assertions '((assert! (rule (?x next-to ?y in (?x ?y . ?u))))
                            (assert! (rule (?x next-to ?y in (?v . ?z))
                                           (?x next-to ?y in ?z)))))

(define queries '((?x next-to ?y in (1 (2 3) 4))
                  ;; Will match multiple rules
                  ;; (?x=1 ?y=(2 3) ?u=(4))
                  ;; (?v=1 ?z=((2 3) 4))
                  (?x next-to 1 in (2 1 3 1))
                  ;; Will match to:
                  ;; (?x=2) and (?x=3)
                  ))

(define program (append assertions custom-assertions queries))

(interpret program)