#lang racket

;; Interleaving serves much the same purpose as in 3.5.3

;; Without interleaving, we may get an infinite number of solutions down a single search path and never find a result.
;; With interleaving, we may traverse down a finite path (eventually) to actually return a solution. Interleaving allows
;; searching of different paths, rather than getting stuck in a dead end.
