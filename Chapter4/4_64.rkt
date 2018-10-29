#lang racket
;; Rule proposed by Louis Reasoner:
;; (rule (outranked-by ?staff-person ?boss)
;;   (or (supervisor ?staff-person ?boss)
;;       (and (outranked-by ?middle-manager ?boss)
;;            (supervisor ?staff-person ?middle-manager))))

;; The issue with this rule is based on the series circuitry of and.
;; The middle manager is unbound in the call to (outranked-by ?middle-manager ?boss)
;; This is a recursive call, and we attempt another call to outranked-by which recurses infinitely.

;; If we switch the order of the circuit, we will find bindings for ?middle-manager. We will recurse,
;; but this time recurse up the chain of command. Eventually, (supervisor ?staff-person ?middle-manager) will fail
;; and our rule will return a result.
