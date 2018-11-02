#lang racket

;; Machine-level procedures
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) (make-mutable ops))
    ((machine 'install-instruction-sequence)
     (assemble (make-mutable controller-text) machine))
    machine))

(define (make-mutable list)
  (if (not (pair? list))
      list
      (foldr (lambda (cur acc)
               (if (pair? cur)
                   (mcons (make-mutable cur) acc)
                   (mcons cur acc))) '() list)))

;; Register procedures
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; Stack procedures
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '()))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (mappend list1 list2)
  (if (null? list1)
      list2
      (mcons (mcar list1)
             (mappend (mcdr list1) list2))))

;; Machine procedures
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (mlist (mlist 'initialize-stack
                         (lambda () (stack 'initialize)))))
          (register-table (mlist (mlist 'pc pc) (mlist 'flag flag))))
      (define (allocate-register name)
        (if (massoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (mcons (mlist name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (massoc name register-table)))
          (if val
              (mcadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (mcar insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (mappend the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

;; The Assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (mcdr text)
                      (lambda (insts labels)
                        (let ((next-inst (mcar text)))
                          (if (symbol? next-inst)
                              (receive insts
                                       (mcons (make-label-entry next-inst
                                                                insts)
                                             labels))
                              (receive (mcons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

(define (for-each-m f m)
  (if (null? m)
      (void)
      (begin (f (mcar m))
             (for-each-m f (mcdr m)))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each-m
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

;; Assembler selectors
(define (make-instruction text)
  (mcons text '()))

(define (instruction-text inst)
  (mcar inst))

(define (instruction-execution-proc inst)
  (mcdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

(define (make-label-entry label-name insts)
  (mcons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (massoc label-name labels)))
    (if val
        (mcdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;; Assembler type analyzer
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (mcar inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (mcar inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (mcar inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (mcar inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (mcar inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (mcar inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (mcar inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (mcar value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (mcadr assign-instruction))
(define (mcddr list) (mcdr (mcdr list)))
(define (assign-value-exp assign-instruction)
  (mcddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (mcdr (get-contents pc))))

;; Assembler conditional
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instructions -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (mcdr test-instruction))

;; Assembler branch
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (mcadr branch-instruction))

;; Assembler goto
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (mcadr goto-instruction))

;; Assembler save
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))


;; Assembler restore
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction) (mcadr stack-instruction))

;; Assember perform
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; Subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

;; Basic selectors
(define (tagged-list? exp tag)
  (and (mpair? exp) (eq? (mcar exp) tag)))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (mcadr exp))

(define (constant-exp? exp) (number? exp))
(define (constant-exp-value exp) exp)

(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (mcadr exp))

(define (mmap f list)
  (if (null? list)
      '()
      (mcons (f (mcar list))
             (mmap f (mcdr list)))))

(define (foldr-m f init list)
  (if (null? list)
      init
      (f (mcar list)
         (foldr-m f init (mcdr list)))))

(define (make-immutable mlist)
  (foldr-m cons '() mlist))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (mmap (lambda (e)
                 (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (make-immutable (mmap (lambda (p) (p)) aprocs))))))

(define (operation-exp? exp)
  (and (mpair? exp)
       (mpair? (mcar exp))
       (symbol? (mcar (mcar exp)))
       (null? (mcdr (mcar exp)))))


(define (mcadr exp) (mcar (mcdr exp)))

(define (operation-exp-op exp) (mcar (mcar exp)))
(define (operation-exp-operands exp) (mcdr exp))

(define (mcaar list) (mcar (mcar list)))

(define (massoc var bindings)
  (cond ((null? bindings) #f)
        ((equal? var (mcaar bindings)) (mcar bindings))
        (else (massoc var (mcdr bindings)))))

;; Lookup
(define (lookup-prim symbol operations)
  (let ((val (massoc symbol operations)))
    (if val
        (mcadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (mlist . vals)
  (foldr mcons '() vals))

;; Syntactical changes:
;; (const 0) => 0
;; (op +) => (+)

;; Test our machine!
(define sqrt-machine-fewer-primitives
  (make-machine
   '(a guess t)
   (list (list '- -)
         (list '/ /)
         (list '* *)
         (list '+ +)
         (list '< <))
   '((assign guess 1.0)
     test-b
     (assign t (*) (reg guess) (reg guess))
     (assign t (-) (reg t) (reg a))
     (test (<) 0 (reg t))
     (branch (label gt))
     (assign t (*) (reg t) -1)
     gt
     (test (<) (reg t) 0.0001)
     (branch (label iter-done))
     (assign t (/) (reg a) (reg guess))
     (assign t (+) (reg t) (reg guess))
     (assign t (/) (reg t) 2)
     (assign guess (reg t))
     (goto (label test-b))
     iter-done
     (assign a (reg guess)))))

(set-register-contents! sqrt-machine-fewer-primitives 'a 4)
(start sqrt-machine-fewer-primitives)
(get-register-contents sqrt-machine-fewer-primitives 'a)


(provide make-machine)
(provide set-register-contents!)
(provide start)
(provide get-register-contents)
