#lang racket
(require "logic.rkt")
(require "database.rkt")

(define custom-assertions '((assert! (meeting accounting (Monday 9am)))
                            (assert! (meeting administration (Monday 10am)))
                            (assert! (meeting computer (Wednesday 3pm)))
                            (assert! (meeting administration (Friday 1pm)))
                            (assert! (meeting whole-company (Wednesday 4pm)))))

(define part-a '(meeting ?any-meeting (Friday . ?any-time)))
(define part-b '(assert! (rule (meeting-time ?person ?day-and-time)
                                (and (job ?person (?division . ?rest-of-job-title))
                                     (or (meeting whole-company ?day-and-time)
                                         (meeting ?division ?day-and-time))))))
(define part-c '(and (meeting-time (Hacker Alyssa P) (Wednesday . ?any-time))
                     (meeting ?div (Wednesday . ?any-time))))

(define queries (list part-a part-b part-c))

(define program (append assertions custom-assertions queries))

(interpret program)