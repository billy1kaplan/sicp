#lang racket

;; Original Implementation: 
;; (define (add-assertion! assertion)
;;   (store-assertion-in-index assertion)
;;   (let ((old-assertions THE-ASSERTIONS))
;;     (set! THE-ASSERTIONS
;;           (cons-stream assertion old-assertions))
;;     'ok))

;; Proposed Implementation: 
;; (define (add-assertion! assertion)
;;   (store-assertion-in-index assertion)
;;   (set! THE-ASSERTIONS
;;         (cons-stream assertion THE-ASSERTIONS))
;;   'ok)


;; The issue with this is that cons-stream is a special form that uses delayed evaluation.
;; Thus, we store this internally as: (cons-stream assertion (lambda () THE-ASSERTIONS))
;; THE-ASSERTIONS has just been set to (cons-stream ...)
;; Thus, we will get an infinite stream with the new insertion placed on front

;; The hint shows this more clearly: (define ones (cons-stream 1 ones))
;; We are in effect doing the same thing with assertion taking the place of 1 and THE-ASSERTIONS being referred
;; to recursively.
