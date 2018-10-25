#lang racket
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
         (list 'eq? eq?)
         (list 'equal? equal?)
         (list 'abs abs)
         (list 'newline newline)
         (list 'and (lambda (a b) (and a b)))
         (list 'or (lambda (a b) (or a b)))
         (list 'or3 (lambda (a b c) (or a b c)))
         (list 'append append)
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
          (error "Too few arguments supplied" vars (length vars) "::" vals (length vals)))))

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
            ((and (pair? vars) (eq? var (car vars)))
             (set-car! vals val))
            ((and (mpair? vars) (eq? var (mcar vars)))
             (set-car! vals val))
            ((mpair? vars) (scan (mcdr vars) (mcdr vals)))
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

(define program '(begin
                   (define require (lambda (p)
                                     (if (not p)
                                         (amb)
                                         'ok)))
                   (define null? (lambda (x)
                                   (eq? x '())))
                   (define member (lambda (obj list)
                                    (cond ((null? list) #f)
                                          ((equal? (car list) obj) #t)
                                          (else (member obj (cdr list))))))
                   (define distinct? (lambda (list)
                                       (cond ((null? list) #t)
                                             ((null? (cdr list)) #t)
                                             ((member (car list) (cdr list)) #f)
                                             (else (distinct? (cdr list))))))
                   (define nouns '(noun student professor cat class))
                   (define verbs '(verb studies lectures eats sleeps))
                   (define articles '(article the a))
                   (define parse-sentence (lambda ()
                                            (list 'sentence
                                                  (parse-noun-phrase)
                                                  (parse-verb-phrase))))
                   (define parse-noun-phrase (lambda ()
                                               (list 'noun-phrase
                                                     (parse-word articles)
                                                     (parse-word nouns))))
                   (define *unparsed* '())
                   (define parse-word (lambda (word-list)
                                        (begin
                                          (require (not (null? *unparsed*)))
                                          (require (member (car *unparsed*) (cdr word-list)))
                                          (let ((found-word (car *unparsed*)))
                                            (begin
                                            (set! *unparsed* (cdr *unparsed*))
                                            (list (car word-list) found-word))))))
                   (define parse (lambda (input)
                                   (set! *unparsed* input)
                                   (let ((sent (parse-sentence)))
                                     (begin (require (null? *unparsed*))
                                            (display sent)
                                            (newline)
                                            (display "---")
                                            (newline)
                                            (amb)))))
                   (define prepositions '(prep for to in by with))
                   (define adjectives '(adj green yellow red big small))
                   (define parse-prepositional-phrase (lambda ()
                                                        (list 'prep-phrase
                                                              (parse-word prepositions)
                                                              (parse-noun-phrase))))
                   (define maybe-extend (lambda (tag phrase)
                                          (amb phrase
                                               (maybe-extend tag (list tag
                                                                       phrase
                                                                       (parse-prepositional-phrase))))))
                   (define parse-verb-phrase (lambda ()
                                               (maybe-extend 'verb-phrase (parse-word verbs))))
                   (define parse-simple-noun-phrase (lambda ()
                                                      (amb (list 'simple-noun-phrase
                                                                 (parse-word articles)
                                                                 (parse-word nouns))
                                                           (list 'adj-simple-noun-phrase
                                                                 (parse-word articles)
                                                                 (parse-word adjectives)
                                                                 (parse-word nouns)))))
                   (define parse-noun-phrase (lambda ()
                                               (maybe-extend 'noun-phrase (parse-simple-noun-phrase))))
                   (parse '(the big professor lectures to the student in the class with the green cat))))
;; Add possible adjective before noun phrases
(define safe? (lambda (k positions)                                 
                (if (= k 1)
                    #t
                    (iter 1 (car positions) (cdr positions)))))

(define iter (lambda (col row remaining-pos)
               (cond ((null? remaining-pos) #t)
                     ((or (= row (car remaining-pos))
                           (= (car remaining-pos) (- row col))
                           (= (car remaining-pos) (+ row col))) #f)
                     (else (iter (+ col 1) row (cdr remaining-pos))))))
     
(ambeval program
         primitive-env
         (lambda (value fail) (display value) (newline))
         (lambda () 'failed))
