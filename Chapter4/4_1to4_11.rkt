#lang racket

;; 4.1

(define (eval exp env)
  ('exp))

(define (no-operands? exps)
  (null? exps))

(define (first-operand exps)
  (car exps))

(define (rest-operands exps)
  (cdr exps))

(define (list-of-values-l exps env)
  (if (no-operands? exps)
      null
      (let ([left (eval (first-operand exps) env)])
        (cons left
              (list-of-values-l (rest-operands exps) env)))))

(define (list-of-values-r exps env)
  (if (no-operands? exps)
      null
      (let ([right (list-of-values-r (rest-operands exps) env)])
        (cons (eval (first-operand exps) env)
              right))))

;; 4.2
;; a). The problem with this approach is that multiple patterns fall under the concept of "procedure application"
;; We would attempt to lookup a value called 'define' in the global environment. However, we need the environment itself for the define procedure.

;; b). If we add the tag 'call...
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (call? exp) (tagged-list? exp 'call))

;; ((call? exp)
;;  (apply (eval (operator exp) env)
;;         (list-of-values (operands exp) env))

;; 4.3

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

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

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
   (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Let (4.6 - 4.7)
(define (let? exp) (tagged-list? exp 'let))
(define (first-binding exp) (car exp))
(define (let-var binding) (car binding))
(define (let-binding binding) (cadr binding))
(define (rest-bindings exp) (cdr exp))
(define (let-bindings exp)
  (car exp))
(define (let-body exp)
  (list (cadr exp)))

(define (named-let? exp)
  (= (length exp) 3))
(define (nlet-name exp) (car exp))
(define (nlet-bindings exp) (cadr exp))
(define (nlet-body exp) (caddr exp))

(define (let->combination exp)
  (define (let-builder vars values bindings body)
    (if (null? bindings)
        (cons (make-lambda vars body) values)
        (let [(first (first-binding bindings))
              (rest (rest-bindings bindings))]
          (let-builder (cons (let-var first) vars)
                       (cons (let-binding first) values)
                       rest
                       body))))
  ;; 4.8
  (define (named-let-builder vars values bindings name body)
    (if (null? bindings)
        (list (list 'define (cons name (reverse vars)) body)
                (cons name (reverse values)))
        (let [(first (first-binding bindings))
              (rest (rest-bindings bindings))]
          (named-let-builder (cons (let-var first) vars)
                       (cons (let-binding first) values)
                       rest
                       name
                       body))))
  (cond [(null? exp) (error "No bindings found")]
        [(not (pair? exp)) (error "Malformed let expression" exp)]
        [(named-let? exp) (named-let-builder null null (nlet-bindings exp) (nlet-name exp) (nlet-body exp))]
        [(= (length (let-bindings exp)) 0) (error "No let bindings present")]
        [else (let-builder null null (let-bindings exp) (let-body exp))]))

;; This can be directly added into the evaluator, no need to explicitly expand
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp);
  (define (let*-builder bindings body)
    (let [(first (list (first-binding bindings)))
          (rest (rest-bindings bindings))]
      (if (null? rest)
          (let->combination (cons first body))
          (let->combination (cons first
                                  (list (let*-builder rest body)))))))
  (cond [(null? exp) (error "No bindings found")]
        [(= (length (let-bindings exp)) 0) (error "No let bindings present")]
        [else (let*-builder (let-bindings exp) (let-body exp))]))

;; Iteration
(define (for-loop? exp)
  (tagged-list? exp 'for))

(define (for-loop start test incrementer body)
  (if (not (test start))
      #f
      (begin (body start) (for-loop (incrementer start) test incrementer body))))

;; Conditionals
(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (eval-if exp env)
  (if (true? (data-eval (if-predicate exp) env))
      (data-eval (if-consequent exp) env)
      (data-eval (if-alternative exp) env)))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (arrow clause) (cadr clause))
(define (arrow-pred clause) (car clause))
(define (arrow-exp clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        ;; Exercise 4.5
        (cond
          [(and (= (length first) 3) (eq? '=> (arrow first)))
           (make-if (arrow-pred first)
                    (list (arrow-exp first) (arrow-pred first))
                    (expand-clauses rest))]
          [(cond-else-clause? first)
           (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE clause isn't last -- COND->IF" clauses))]          
          [else (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))]))))

(define (application? exp)
  (pair? exp))

(define (list-of-values exps env)
  (if (no-operands? exps)
      null
      (cons (data-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (application exp)
  (pair? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (operator exp)
  (car exp))

(define (operands exp) (cdr exp))

(define (text-of-quoted exp) (caddr exp))

;(define (lookup-variable-value exp env)
;  (cond [(null? env) (error "Unbound variable")]
;        [(eq? (car exp) (caar env)) (cadr env)]
;        [else (lookup-variable-value exp (cdr env))]))

(define (data-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp)) (data-eval (apply (get 'eval (car exp)) exp env) env))
        ((let? exp) (data-eval (let->combination exp)))
        ((let*? exp) (let*->nested-lets exp))
        ((application? exp)
         (apply
          (eval (operator exp) env)
          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type" exp))))

(define (自评估？ 表达)
  (or (number? 表达)
      (string? 表达)))

(define (when-condition exp)
  (cadr exp))

(define (when-exp exp)
  (caddr exp))

(define (eval-when exp env)
  (make-if (when-condition exp)
           (when-exp exp)
           #f))

(define (评估 表达 环境)
  (cond [(自评估？ 表达)
         表达]
        [#t 1]))

;(define (apply procedure arguments)
;  (cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure procedure arguments))
;        ((compound-procedure? procedure)
;         (eval-sequence
;          (procedure-body procedure)
;          (extend-environment
;           (procedure-parameters procedure)
;           arguments
;           (procedure-environment procedure))))
;        (else
;         (error
;          ("Unknown procedure type -- APPLY" procedure)))))

(define (get key1 key2)
  (hash-ref ops (cons key1 key2)))

(define (put key1 key2 value)
  (hash-set! ops (cons key1 key2) value))

(define ops (make-hash))

(define (and-clauses exp) (cdr exp))

(define (true? exp)
  exp)

(define (check-and clauses env)
  (if (null? clauses)
      true
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (true? (data-eval first env))
            (check-and rest env)
            false))))
                                                        
(define (eval-or clauses env)
  (if (null? clauses)
      false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (true? (data-eval first env))
            true
            (eval-or rest env)))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (eval-and-derived clauses)
  (if (null? clauses)
      true
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 (eval-and-derived rest)
                 false))))


(define (eval-or-derived clauses)
  (if (null? clauses)
      false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 true
                 (eval-or-derived rest)))))

;; Dispatch Table
(put 'eval 'if eval-if)
(put 'eval 'cond cond->if)
(put 'eval 'and eval-and-derived)
(put 'eval 'or eval-or-derived)

;; Environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment (list))

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cons [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (car vals)]
            [#t (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let [(frame (first-frame env))]
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable SET!" var)
        (let [(frame (first-frame env))]
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (car vars))
             (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Paired environment: 4.11
(define (make-frame-pairs var-vals)
  var-vals)

(define (frame-variables-pairs frame)
  (map mcar frame))

(define (frame-values-pairs frame)
  (map mcdr frame))

(define (binding-var binding)
  (mcar binding))

(define (binding-value binding)
  (mcdr binding))

(define (add-binding-to-frame-pairs! var val frame)
  (set-mcar! frame (mcons (mcons var val) frame)))

(define (lookup-variable-value-pairs var env)
  (define (env-loop env)
    (define (scan bindings)
      (cons [(null? bindings)
             (env-loop (enclosing-environment env))]
            [(eq? var (binding-var (mcar bindings)))
             (binding-value (mcar bindings))]
            [#t (scan (mcdr bindings))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let [(frame (first-frame env))]
          (scan frame))))
  (env-loop env))

(define (set-variable-value-pairs! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond [(null? bindings)
             (env-loop (enclosing-environment env))]
            [(eq? var (binding-var (mcar bindings)))
             (set-mcdr! (mcar bindings) val)]
            [else (scan (mcdr bindings))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable SET!" var)
        (let [(frame (first-frame env))]
          (scan frame))))
  (env-loop env))

(define (define-variable-pairs! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond [(null? bindings)
             (add-binding-to-frame! var val frame)]
            [(eq? var (binding-var (car bindings)))
             (set-mcdr! (mcar  bindings) val)]
            [else (scan (mcdr bindings))]))
    (scan frame)))