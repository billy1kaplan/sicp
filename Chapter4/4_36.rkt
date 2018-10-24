#lang racket

;; Why we cannot simply replace an-integer-between with an-integer-starting-from to generate arbitrary Pythagorean triples
;; Assuming we simply replace the first value, i, with 'an-integer-starting-from'
;; We will stop hitting our failure conditions... since we keep counting up we will try for larger and larger values of i without trying other values
;; Since we are performing depth-first search, we continue deeper and deeper down a single path until exhausting the infinite number of natural numbers
;; before using one closer to "home"

;; Exponentially search a larger and larger space:
;; Assumes we start searching from 1
; (define (a-pythagorean-triple)
;   (define (a-pythagorean-triple-upto n)
;     (let ((high (* 2 n)))
;       (let ((i (an-integer-between 1 high)))
;         (let ((j (an-integer-between i high)))
;           (let ((k (an-integer-between n high)))
;             (require (= (+ (* i i) (* j j)) (* k k)))
;             (list i j k))))
;    (define (search n)
;      (amb (a-pythagorean-triple-upto n)
;           (search (* 2 n))))
;    (search 1))

;; Nice solution found online:
; (define (a-pythagorean-triple-from low)
;   (let ((k (an-integer-starting-from low)))
;     (let ((j (an-integer-between low k)))
;       (let ((i (an-integre-between low j)))
;         (require (= (+ (* i i) (* j j)) (* k k)))
;         (list i j k)))

(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)

(define (true? exp)
  (not (or (eq? exp '#f)
           (eq? exp '#false)
           (eq? exp 'false))))

;; Type-tag checking
;; General contract: exp -> boolean
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
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
(define (make-if exp)
  (list 'if (if-predicate exp) (if-consequent exp) (if-alternative exp)))

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
(define (last-exp? seq) (null? cdr seq))
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

(define (mlist . vals)
  (define (internal vals)
    (if (null? vals)
        '()
        (mcons (car vals)
               (internal (cdr vals)))))
  (internal vals))

(define (mmap f list)
  (if (null? list)
      '()
      (mcons (f (mcar list))
             (mmap f (mcdr list)))))

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

;; Analysis
;; General contract: exp -> env -> exp
;; "Curried" to take the exp during analysis and then the env at runtime
;(define (analyze-self-evaluating exp)
;  (lambda (env) exp))
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;(define (analyze-quoted exp)
;  (let ((qval (text-of-quotation exp)))
;    (lambda (env) qval)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

; If a variable is undefined at runtime, this is a programmatic bug; this does not signal continuing our non-deterministic search!
;(define (analyze-variable exp)
;  (lambda (env) (lookup-variable-value exp env)))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

;(define (analyze-assignment exp)
; (let ((var (assignment-variable exp))
;        (vproc (analyze (assignment-value exp))))
;    (lambda (env)
;      (set-variable-value! var (vproc env) env)
;      'ok)))
(define (analyze-assignment exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            ;; if we fail here, perform special assignment failure, and then we keep failing until our failure is handled differently
                            (fail2)))))
             fail))))

;(define (analyze-definition exp)
;  (let ((var (definition-variable exp))
;        (vproc (analyze (definition-value exp))))
;    (lambda (env)
;      (define-variable! var (vproc env) env)
;      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
               fail))))

;(define (analyze-if exp)
;  (let ((pproc (analyze (if-predicate exp)))
;        (cproc (analyze (if-consequent exp)))
;        (aproc (analyze (if-alternative exp))))
;    (lambda (env)
;      (if (true? (pproc env))
;          (cproc env)
;          (aproc env)))))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; if we successfully can evaluate our pproc, then we attempt to continue with the if logic
             ;; We write a custom if expression success handler
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

;(define (analyze-lambda exp)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence (lambda-body exp))))
;    (lambda (env) (make-procedure vars bproc env))))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

    
;(define (analyze-sequence exps)
;  (define (sequentially proc1 proc2)
;    (lambda (env) (proc1 env) (proc2 env)))
;  (define (loop first-proc rest-procs)
;    (if (null? rest-procs)
;        first-proc
;        (loop (sequentially first-proc (car rest-procs))
;              (cdr rest-procs))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs)
;        (error "Empty sequence -- ANALYZE")
;        (loop (car procs) (cdr procs)))))
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; handle a successful call to a, a custom success handler that attempts to get the b-value
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; if we fail in our call to a, use the fail handler
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (loop (car procs) (cdr procs)))))
        
;(define (analyze-application exp)
;  (let ((fproc (analyze (operator exp)))
;        (aprocs (map analyze (operands exp))))
;    (lambda (env)
;      (execute-application (fproc env)
;                           (map (lambda (aproc) (aproc env))
;                                aprocs)))))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

;(define (execute-application proc args)
;  (cond ((primitive-procedure? proc)
;         (apply-primitive-procedure proc args))
;        ((compound-procedure? proc)
;         ((procedure-body proc)
;          (extend-environment (procedure-parameters proc)
;                              args
;                              (procedure-environment proc))))
;        (else
;         (error "Unknown procedure type -- EXECUTE-APPLICATION"
;                proc))))
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION"
                proc))))

;; Define amb evaluator
(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;; Main analysis function
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define primitive-env (mlist (make-frame (primitive-procedure-names)
                                         (primitive-procedure-objects))))

; Example procedure from text:
;(lambda (env succeed fail)
  ;; succeed is (lambda (value fail) ...)
  ;; fail is (lambda () ...)
  ;;...)

; Example call of ambevel
; (ambeval <exp>
;   the-global-environment
;   (lambda (value fail) value)
;   (lambda () 'failed))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (prompt-for-input prompt)
  (display prompt)
  (newline))

(define (announce-output output)
  (display output)
  (newline))

;; We will keep searching larger and larger spaces for pythagorean triplets
(define program '(begin
                   (define require (lambda (p)
                                     (if (not p)
                                         (amb)
                                         'ok)))
                   (define an-integer-between (lambda (low high)
                     (require (> high low))
                     (amb low (an-integer-between (+ low 1) high))))
                   (define a-pythagorean-triple (lambda ()
                                                  (define a-pythagorean-triple-upto (lambda (n)
                                                                                      (let ((high (* 2 n)))
                                                                                        (let ((i (an-integer-between 1 high)))
                                                                                          (let ((j (an-integer-between i high)))
                                                                                            (let ((k (an-integer-between n high)))
                                                                                              (begin (require (= (+ (* i i) (* j j)) (* k k)))
                                                                                              (list i j k))))))))
                                                  (define search (lambda (n)
                                                                   (amb (a-pythagorean-triple-upto n)
                                                                        (search (* 2 n)))))
                                                  (search 1)))
                   (a-pythagorean-triple)))

(define (driver-loop)
  (define (internal-loop try-again)
    (display "Awaiting commands")
    (newline)
    (let ((input (read-line)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (ambeval
             program
             primitive-env
             (lambda (val next-alternative)
               (display val)
               (newline)
               (internal-loop next-alternative))
             (lambda ()
               'done))))))
  (internal-loop
   (lambda ()
     (newline)
     (driver-loop))))

(ambeval program
         primitive-env
         (lambda (value fail) (display value) (newline))
         (lambda () 'failed))