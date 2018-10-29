#lang racket
(require "logic.rkt")

(define custom-assertions '((assert! (son Adam Cain))
                            (assert! (son Cain Enoch))
                            (assert! (son Enoch Irad))
                            (assert! (son Irad Mehujael))
                            (assert! (son Mehujael Methushael))
                            (assert! (son Methushael Lamech))
                            (assert! (wife Lamech Ada))
                            (assert! (son Ada Jabal))
                            (assert! (son Ada Jubal))

                            (assert! (rule (son-of ?p ?s)
                                           (or (son ?p ?s)
                                               (son-of-wife ?p ?s))))
                            (assert! (rule (son-of-wife ?man ?s)
                                           (and (wife ?man ?w)
                                                (son ?w ?s))))
                            (assert! (rule (grandson ?g ?gs)
                                           (and (son-of ?g ?p)
                                                (son-of ?p ?gs))))))

(define queries '((grandson Cain ?g) ;; Find the grandson of Cain
                  (son-of Lamech ?s) ;; All the sons of Lamech
                  (grandson Methushael ?g)
                  )) 

(define program (append custom-assertions queries))

(interpret program)