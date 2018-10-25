#lang racket

; I believe the result will depend on the implementation of prime-sum-pair:
; If the function just returns a single value, without amb, then it will be the single valued list '(val)
; Otherwise, the result will be all of the values produced by the prime-sum-pair function since (amb)
; will force all the branches of failure to be explored. These values are stored since permanent-set!
; does not rollback on failure.
