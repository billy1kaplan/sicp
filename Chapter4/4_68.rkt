#lang racket
(require "logic.rkt")

(define custom-assertions '((assert! (rule (append-to-form () ?y ?y)))
                            (assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                                           (append-to-form ?v ?y ?z)))
                            (assert! (rule (reverse () ())))
                            (assert! (rule (reverse (?first . ?rest) ?rev)
                                           (and (reverse ?rest ?rev-rest)
                                                (append-to-form ?rev-rest (?first) ?rev))))))

(define queries '((reverse () ?x)
                  (reverse (1 2 3) ?x)
                  '(reverse ?x (1 2 3)))) ;; This one never finishes. We need to hit our base case of () for the second list.
                                          ;; This doesn't happen because we never know when we hit this for an unbound list.
(define program (append custom-assertions queries))

(interpret program)