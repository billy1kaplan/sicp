#lang racket
;; Diagram:
;; CALLING ENV -> proc (lambda env) -> let (lambda env) <- set vars
;;             -> proc (lambda env) <- set vars

;; With this substitution we create an additional frame through the usage of let (which will become a lambda expression in the interpreter)
;; As an alternative, the procedure environment could directly be updated without the additional frame

;; Initial idea
; (let ((u '*unassigned*)
;       (v '*unassigned*))
;   (set! u <e1>)
;   (set! v <e2>)
;   (<e3>))
;
;; Alternative
; ((define u '*unassigned*)
;  (define v '*unassigned*)
;  (set! u <e1>)
;  (set! v <e1>)
;  <e3>)