#lang racket

(require "simulator-stack-trace.rkt")

;; "Primitives"
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)

(define (mlist . args)
  (foldr mcons '() args))

(define (mmap f args)
  (if (null? args)
      '()
      (mcons (f (mcar args))
             (mmap f (mcdr args)))))

(define (true? exp)
  (not (or (eq? exp '#f)
           (eq? exp '#false)
           (eq? exp 'false))))

;; Type-tag checking
;; General contract: exp -> boolean
(define (tagged-list? exp tag)
  (or (and (mpair? exp) (eq? (mcar exp) tag))
      (and (pair? exp) (eq? (car exp) tag))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((eq? exp '#t) #t)
        ((eq? exp '#f) #t)
        (else #f)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (cadddr exp))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; Let
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-bindings exp)
  (cadr exp))
(define (first-binding bindings)
  (car bindings))
(define (rest-bindings bindings)
  (cdr bindings))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cadr binding))
(define (let-body exp)
  (caddr exp))
(define (let->combination exp)
  (cons (make-lambda (map binding-var (let-bindings exp))
                     (list (let-body exp)))
        (map binding-val (let-bindings exp))))

(define assert-equal (lambda (text actual expected)
                       (if (equal? actual expected)
                           (string-append "PASS -- " text)
                           (error "TEST FAILED" actual expected))))

;; Procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-environment p)
  (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (mlist (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         ;(list 'null? '())
         (list '+ +)
         (list '- -)
         (list '* *)
         (list '/ /)
         (list '= =)
         (list '< <)
         (list '> >)
         (list '<= <=)
         (list '>= >=)
         (list 'not not)
         (list 'list list)
         (list 'display display)
         (list 'eq? eq?)
         (list 'equal? equal?)
         (list 'abs abs)
         (list 'newline newline)
         ))

(define (primitive-procedure-names)
  (mmap car primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

;; Environment functions
(define (enclosing-environment env)
  (mcdr env))

(define (first-frame env)
  (mcar env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame)
  (mcar frame))

(define (frame-values frame)
  (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; This is terrible!
;; Our environment can contain a mix of mutable and immutable cons pairs
;; In-truth; we don't care
;; Ideally, there should be a macro to treat everything as mutable cons pairs
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((and (pair? vars) (eq? var (car vars))) (car vals))
            ((and (mpair? vars) (eq? var (mcar vars))) (mcar vals))
            ((pair? vars) (scan (cdr vars) (cdr vals)))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; Definition
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (prompt-for-input string)
  (newline) (display string) (newline))

(define (get-global-environment) primitive-env)

(define (announce-output output)
  (newline) (display output) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define primitive-env (mlist (make-frame (primitive-procedure-names)
                                         (primitive-procedure-objects))))

;; Evaluator
(define (evaluator)
  (make-machine
   '(exp env val continue proc arg1 unev)
   `((empty-arglist ,empty-arglist) (adjoin-arg ,adjoin-arg) (last-operand? ,last-operand?)
                                    (self-evaluating? ,self-evaluating?) (variable? ,variable?) (quoted? ,quoted?) (assignment? ,assignment?)
                                    (definition? ,definition?) (if? ,if?) (lambda? ,lambda?) (begin? ,begin?) (application? ,application?)
                                    (lookup-variable-value ,lookup-variable-value) (text-of-quotation, text-of-quotation)
                                    (lambda-parameters ,lambda-parameters) (lambda-body ,lambda-body)
                                    (make-procedure ,make-procedure) (operands ,operands) (operator ,operator)
                                    (no-operands? ,no-operands?) (first-operand ,first-operand)
                                    (last-operand? ,last-operand?) (rest-operands ,rest-operands)
                                    (primitive-procedure? ,primitive-procedure?)
                                    (compound-procedure? ,compound-procedure?)
                                    (apply-primitive-procedure ,apply-primitive-procedure)
                                    (procedure-parameters ,procedure-parameters)
                                    (procedure-environment ,procedure-environment)
                                    (extend-environment ,extend-environment)
                                    (procedure-body ,procedure-body)
                                    (begin-actions ,begin-actions)
                                    (first-exp ,first-exp)
                                    (last-exp? ,last-exp?)
                                    (rest-exps ,rest-exps)
                                    (if-predicate ,if-predicate)
                                    (if-consequent ,if-consequent)
                                    (if-alternative ,if-alternative)
                                    (true? ,true?)
                                    (assignment-variable ,assignment-variable)
                                    (assignment-value ,assignment-value)
                                    (set-variable-value! ,set-variable-value!)
                                    (definition-variable ,definition-variable)
                                    (definition-value ,definition-value)
                                    (define-variable! ,define-variable!)
                                    (prompt-for-input ,prompt-for-input)
                                    (read ,read)
                                    (get-global-environment ,get-global-environment)
                                    (announce-output ,announce-output)
                                    (user-print ,user-print)
                                    (display ,display)
                                    (symbol? ,symbol?))
   '((assign continue (label eval-done))
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (assign unev (op operands) (reg exp))
     (assign exp (op operator) (reg exp))
     (test (op symbol?) (reg exp))
     (save unev)
     (branch (label ev-appl-symbol-operator))
     (save env)
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-symbol-operator
     (assign continue (label ev-appl-did-symbol-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore env)
     ev-appl-did-symbol-operator
     (restore unev)
     (assign arg1 (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     ev-appl-operand-loop
     (save arg1)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-apply-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore arg1)
     (assign arg1 (op adjoin-arg) (reg val) (reg arg1))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-apply-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore arg1)
     (assign arg1 (op adjoin-arg) (reg val) (reg arg1))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg arg1))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg arg1) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ;; A simple tail recursion implementation
     ;; Allows us to use constant space rather than space proportional to the number of recursive calls when our procedure is in the tail position!

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp)) ;; Doing this branch is enough to implement tail recursion! We don't add to the stack 
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
	  
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (reg continue))

     eval-done)))

(provide evaluator)

;; Test for part A
(define symbol-app '(+ 1 1))

(define f-app '(((lambda () +)) 1 1))

(define (run-test-with-stats prog)
  (let ((evaluator (evaluator)))
    (set-register-contents! evaluator 'exp prog)
    (set-register-contents! evaluator 'env (get-global-environment))
    (start evaluator)
    (newline)
    (display "Results: ") (display (get-register-contents evaluator 'val))
    (print-stats evaluator)))

(run-test-with-stats symbol-app)
(run-test-with-stats f-app)

;; Part B
;; By extending the evaluator to handle more and more cases.
;; We may be able to achieve some efficiency gains by replacing expensive
;; operations (such as on the stack) with cheaper operations in the interpreter.
;; However, the compiler will make a better optimization. By moving operations
;; from runtime to compile time, we gain performance without making runtime tradeoffs.
;; Also, we need to take apart expressions multiple times to figure out what to do with them
;; whereas the compiler will only do this once which is far more efficient.