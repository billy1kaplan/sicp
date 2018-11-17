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
                                                (son-of ?p ?gs))))
                            (assert! (rule ((grandson) ?g ?s)
                                           (grandson ?g ?s)))
                            
                            (assert! (rule (ends-with-grandson (grandson))))                            
                            (assert! (rule (ends-with-grandson (?great . ?rel))
                                           (ends-with-grandson ?rel)))


                            (assert! (rule ((great . ?rel) ?x ?y)
                                           (and (ends-with-grandson ?rel)
                                                (?rel ?son-x ?y)
                                                (son-of ?x ?son-x)
                                                )))))


(define queries '((ends-with-grandson (grandson))
                  (ends-with-grandson (great great great grandson))
                  
                  ((grandson) ?g ?gs)
                  ((great grandson) ?g ?ggs)
                  ((great great grandson) ?g ?gggs)

                  (?relationship Adam Irad) ;; This query causes an infinite loop. We unify with our (great . ?rel) rule, but this continues forever
                  ((great grandson) Adam ?ggs)
                  )) 

(define program (append custom-assertions queries))

(interpret program)