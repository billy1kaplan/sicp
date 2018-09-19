#lang racket
; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))

;; If the definition of a came before b, we would suspect the expression would evaluate to 20. Internal defines are treated as simultaneous definitions. We should thus expect that the order should not matter.

; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a (+ b x)
;     (+ a b))
;   (f 10))


;; Both a & b are undefined. There is no possible way to perform this simultaneous evaluation.

;; In the case we can perform this, we need to basically evaluate the "dependencies" of the expression. This could probably be solved by performing a topological sort to order the expressions and then evaluate in order.
;; Cyclical dependencies cannot be allowed as seen in the example above. In this case, we would expect the evaluator to throw an exception.
;; Generating such a graph would be difficult as we don't actually know the order of "dependencies" for these definitions. We would need to somehow ignore values with delays. This seems like ugly behavior to support in our evaluator
;; probably is not necessary.