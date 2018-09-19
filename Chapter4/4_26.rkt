#lang racket

; (unless cond exceptional normal)
(define (make-if if-predicate if-consequent if-alternative)
  (list 'if if-predicate if-consequent if-alternative))

(define (unless->if exp)
  (make-if (cadr exp) (cadddr exp) (caddr exp)))

(define test-input '(unless (= n 1) (* n n) 2))
(define expected-output '(if (= n 1) 2 (* n n)))
(equal? expected-output (unless->if test-input))

;; We might want a procedure instead of a special form, could pass around if or unless as a precondition check?