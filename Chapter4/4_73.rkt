#lang racket

;; Original:
;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (interleave-delayed
;; 	   (stream-car stream)
;; 	   (delay (flatten-stream (stream-cdr stream))))))
;; 

;; Proposed:
;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (interleave
;; 	   (stream-car stream)
;; 	   (flatten-stream (stream-cdr stream)))))

;; This will attempt to evaluate stream-cdr of stream in order to evaluate
;; interleave and recursively evaluate flatten-stream.

;; This could lead to infinite recursion and failure.
