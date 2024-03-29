#lang racket
(require "simulator.rkt")

(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)

;; Selectors:
(define (tagged-list? exp tag)
  (or (and (mpair? exp) (eq? (mcar exp) tag))
      (and (pair? exp) (eq? (car exp) tag))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((eq? exp '#t) #t)
        ((eq? exp '#f) #t)
        (else #f)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (variable? exp)
  (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (mlist . args)
  (foldr mcons '() args))
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
         ))

(define (primitive-procedure-names)
  (mmap car primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

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

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))

;; Primitives:
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((and (pair? vars) (eq? var (car vars))) (car vals))
            ((and (mpair? vars) (eq? var (mcar vars)) (pair? vals)) (car vals))
            ((and (mpair? vars) (eq? var (mcar vars))) (mcar vals))
            ((pair? vars) (scan (cdr vars) (cdr vals)))
            ((and (mpair? vars) (pair? vals)) (scan (mcdr vars) (cdr vals)))
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

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc)
  (cadr c-proc))
(define (compiled-procedure-env c-proc)
  (caddr c-proc))

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


(define primitive-env (mlist (make-frame (primitive-procedure-names)
                                         (primitive-procedure-objects))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (mcons (make-frame vars vals) base-env))

;; Register manipulation
(define all-regs '(env proc val arg1 continue))

;; Instructions
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                   (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
            (if (null? seqs)
                (empty-instruction-sequence)
                (append-2-sequences (car seqs)
                                    (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                          (list-union (list first-reg)
                                      (registers-needed seq1))
                          (list-difference (registers-modified seq1)
                                           (list first-reg))
                          (append `((save ,first-reg))
                                  (statements seq1)
                                  `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))

;; Labels
(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ label-counter 1))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

;; Compiler
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
		 (make-instruction-sequence '(continue) '()
									'((goto (reg continue)))))
		((eq? linkage 'next)
		 (empty-instruction-sequence))
		(else
		  (make-instruction-sequence '() '()
									 `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluation exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() 
                                               (list target)
                                               `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '()
                                               (list target)
                                               `((assign ,target ,(text-of-quotation exp))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '(env) (list target)
                                               `((assign ,target
                                                         (op lookup-variable-value)
                                                         (const ,exp)
                                                         (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op set-variable-value!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp) target consequent-linkage))
            (a-code (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                      (make-instruction-sequence '(val) '()
                                                 `((test (op false?) (reg val))
                                                   (branch (label ,f-branch))))
                      (parallel-instruction-sequences
                       (append-instruction-sequences t-branch c-code)
                       (append-instruction-sequences f-branch a-code))
                      after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next)
                              after-lambda
                              linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
                            (make-instruction-sequence '(env) (list target)
                                                        `((assign ,target
                                                                  (op make-compiled-procedure)
                                                                  (label ,proc-entry)
                                                                  (reg env)))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc arg1) '(env)
                                 `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg arg1)
                                            (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(arg1)
                                   `((assign arg1 (const ()))))
        (let ((code-to-get-last-arg
                (append-instruction-sequences
                  (car operand-codes)
                  (make-instruction-sequence '(val) '(arg1)
                                             '((assign arg1 (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                            (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg 
          (preserving '(arg1)
                      (car operand-codes)
                      (make-instruction-sequence '(val arg1) '(arg1)
                                                 '((assign arg1
                                                           (op cons) (reg val) (reg arg1)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next)
                                after-call
                                linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
                                   `((test (op primitive-procedure?) (reg proc))
                                     (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
                              (make-instruction-sequence '(proc arg1)
                                                          (list target)
                                                          `((assign ,target
                                                                    (op apply-primitive-procedure)
                                                                    (reg proc)
                                                                    (reg arg1)))))))
        after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry) (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    '((assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluation exp target linkage))
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
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(provide compile)

(define (compiler-machine prog)
  (make-machine
   '(exp env val continue proc arg1 unev)
   `((lookup-variable-value ,lookup-variable-value)
     (set-variable-value! ,set-variable-value!)
     (define-variable! ,define-variable!)
     (false? ,false?)
     (make-compiled-procedure ,make-compiled-procedure)
     (compiled-procedure-env ,compiled-procedure-env)
     (extend-environment ,extend-environment)
     (list ,list)
     (cons ,cons)
     (primitive-procedure? ,primitive-procedure?)
     (apply-primitive-procedure ,apply-primitive-procedure)
     (compiled-procedure-entry ,compiled-procedure-entry))
   prog))

(define (reset-compiler)
  (set! label-counter 0))

(define (set-label-start! n)
  (set! label-counter n))

(provide compiler-machine)
(provide reset-compiler)
(provide primitive-env)
(provide statements)
(provide set-label-start!)
