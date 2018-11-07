#lang racket

;; Saves/restores in the interpreter:
;; 1). operator
;; 2). env for each operand
;; 3). arg1 for each operand
;; 4). proc for operand

;; (f 'x 'y)
;; 1 is unnecessary (just a simple lookup).
;; 2 is unnecessary (don't need env for quoted exp)
;; 3 is unnecessary (arg1 not used to eval quoted exp)
;; 4 is unnecessary (no assignments)

;; ((f) 'x 'y)
;; 2 and 3 are unnecessary (quoted expressions have no impact)
;; 1 is unnecessary, it won't be affected by operands
;; proc is unnessary since it's unaffacted by the env

;; (f (g 'x) y)
;; 1). Would be overwritten by applying (g 'x), so we do need the save
;; 2). Yes, the environment could be changed to affect the definition of y. 
;; 3). arg1 should be saved or else it will be impacted by 'x in the application of (g 'x)
;; 4). Proc should be saved or else it will be overwritten by (g 'x)

;; (f (g 'x) 'y)
;; 1). Would be overwritten by applying (g 'x), so we do need the save
;; 2). No, 'y not affected by env.
;; 3). arg1 should be saved or else it will be impacted by 'x in the application of (g 'x)
;; 4). Proc should be saved or else it will be overwritten by (g 'x)
