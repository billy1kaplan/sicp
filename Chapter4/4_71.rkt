#lang racket

;; Original:
;; (define (simple-query query-pattern frame-stream)
;;   (stream-flatmap
;;     (lambda (frame)
;;       (stream-append-delayed
;; 	  (find-assertions query-pattern frame)
;; 	  (delay (apply-rules query-pattern frame))))
;;     frame-stream))
;; 
;; (define (disjoin disjuncts frame-stream)
;;   (if (empty-disjunction? disjuncts)
;;       the-empty-stream
;;       (interleave-delayed
;;         (qeval (first-disjunct disjuncts) frame-stream)
;;         (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

;; Modified:
;; (define (simple-query query-pattern frame-stream)
;;   (stream-flatmap
;;     (lambda (frame)
;;       (stream-append (find-assertions query-pattern frame)
;; 		     (apply-rules query-pattern frame)))
;;     frame-stream))
;; 
;; (define (disjoin disjuncts frame-stream)
;;   (if (empty-disjunction? disjuncts)
;;       the-empty-stream
;;       (interleave
;;         (qeval (first-disjunct disjuncts) frame-stream)
;; 	   (disjoin (rest-disjoins disjuncts) frame-stream))))


;; If we have an infinite number of rules or an invalid infinitely looping rule, for example, we can pull a single frame all the way through, to get some
;; sort of useful output. Without the delay, we will need to perform infinite work (which will never complete) to get a single output (which may be useful!)
