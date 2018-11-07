#lang racket
(define (tagged-list? exp tag)
  (or (and (mpair? exp) (eq? (mcar exp) tag))
      (and (pair? exp) (eq? (car exp) tag))))

define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((eq? exp '#t) #t)
        ((eq? exp '#f) #t)
        (else #f)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (variable? exp)
  (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp)
  (tagged-list? exp 'if))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda? exp)
  (tagged-list? exp 'begin))

(define (application? exp)
  (pair? exp))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
		 (compile-self-evaluating exp target linkage))
		((quoted? exp) (compile-quoted exp target linkage))
		((variable? exp)
		 (compile-variable exp target linkage))
		((assignment? exp)
		 (compile-assignment exp target linkage))
		((definition? exp)
		 (compile-definition exp target linkage))
		((if? exp) (compile-if exp target linkage))
		((lambda? exp) (compile-lambda exp target linkage))
		((begin? exp)
		 (compile-sequencce (begin-actions exp)
							target
							linkage))
		((cond? exp) (compile (cond->if exp) (target linkage)))
		((application? exp)
		 (compile-application exp target linkage))
		(else
		  (error "Unknown expression type -- COMPILE" exp))))
