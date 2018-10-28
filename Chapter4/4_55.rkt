#lang racket
(require "logic.rkt")
(require "database.rkt")

(define part-a '(supervisor ?person (Bitdiddle Ben)))
(define part-b '(job ?person (accounting . ?role)))
(define part-c '(address ?person (Slumerville . ?rest-of-address)))

(define queries (list part-a part-b part-c))

(define program (append assertions
                        queries))

(interpret program)