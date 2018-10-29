#lang racket

;; May need to come back to this one later

;; Anchoring to basic namespace
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;; Defining syntax rules for delayed stream evaluation
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;; Scheme Stream API
(define (force exp) (exp))

(define the-empty-stream '())

(define (stream-null? s)
  (null? s))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                            (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (void)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each (lambda (el) (display el) (newline)) s))

;; Table and Table API
(define (make-table)
  (let ((values (mcons (mcons '() '()) '())))
    (define (get-key value) (mcar value))
    (define (get-value value) (mcdr value))
    (define (get key)
      (define (get-internal key value-list)
        (cond ((null? value-list) #f)
              ((equal? key (get-key (mcar value-list)))
               (mcar value-list))
              (else (get-internal key (mcdr value-list)))))
      (get-internal key values))
    (define (get-immutable key)
      (let ((result (get key)))
        (if result
            (get-value result)
            #f)))
    (define (put key value)
      (let ((result (get key)))
        (if result
            (set-mcdr! result value)
            (set-mcdr! values (mcons (mcons key value)
                                     (mcdr values))))))
    (define (print)
      (define (print-internal vals)
        (cond ((null? vals) (newline))
              ((eq? (mcar (mcar vals)) '()) (print-internal (mcdr vals)))
              (else (begin (display (mcar (mcar vals))) (display " : ") (display (mcdr (mcar vals)))
                            (newline)
                            (print-internal (mcdr vals))))))
      (display "Keys") (display " : ") (display "Values")
      (newline)
      (print-internal values))
    (define (me request)
      (cond ((eq? request 'put) put)
            ((eq? request 'get) get-immutable)
            ((eq? request 'print) print)
            (else (error "Unknown procedure called -- TABLE" request))))
    me))

;; Top Level Table actually used by our evaluator
(define +TABLE+ (make-table))

;; Puts a record into the table corresponding to the keys, modifying an existing
;; record if it exists
(define (put key1 key2 value)
  ((+TABLE+ 'put) (cons key1 key2) value))

;; Gets a record based off the two keys,
;; If the record does not exist, evaluates to false
(define (get key1 key2)
  ((+TABLE+ 'get) (cons key1 key2)))

;; prints the contents of the table
(define (print-table)
  ((+TABLE+ 'print)))

;; Logic Programming Driver Loop
(define input-prompt ";;; Query Input:")
(define output-prompt ";;; Query Results:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

;; Query Syntax Support
(define (tagged-list? list tag)
  (if (pair? list)
      (eq? (car list) tag)
      #f))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
                  (list '?
                        (string->symbol
                         (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

;; Unique Rule ID
(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ rule-counter 1))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                   (if (number? (cadr variable))
                       (string-append (symbol->string (caddr variable))
                                      "-"
                                      (number->string (cadr variable)))
                       (symbol->string (cadr variable))))))

;; Bindings and Frames
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))



;; Driver Loop
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                 frame
                 (lambda (v f)
                   (contract-question-mark v)))))
            (qeval q (singleton-stream '())))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;; Query Language Evaluator
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; Simple query
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;; Rule Loop Detection:
;; Approach: Maintain a rule call stack, if a call does not make
;; progress over an existing call in the stack, we fail

(define (make-stack)
  (let ((stack (mcons '() '())))
    (define (push obj)
      (set! stack (mcons obj stack)))
    (define (pop)
      (if (null? (mcar stack))
          (void)
          (let ((first (mcar stack)))
            (set! stack (mcdr stack))
            first)))
    (define (contains obj)
      (define (contains-internal values)
        (cond ((null? values) #f)
              ((equal? obj (mcar values)) #t)
              (else contains-internal (mcdr values))))
      (contains-internal stack))
    (define (me request)
      (cond ((eq? request 'push) push)
            ((eq? request 'pop) pop)
            ((eq? request 'contains) contains)
            (else (error "Unknown stack operation" request))))
    me))

;; Rules, rules, rules...
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (display "DONE WITH MATCH ")
      (display rule)
      (newline)
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (parse val)
  (cond ((var? val) (caddr val))
        (else val)))

(define count 0)

(define (unify-match p1 p2 frame)
  (define (extend-history p1 p2 history)
    (cons (cons p1 p2) history))
  (define (internal p1 p2 history frame)
    (set! count (+ count 1))
    (display "*** ") (display count) (newline)
    (display "P1: ") (display p1) (newline)
    (display "P2: ") (display p2) (newline)
    (display "History: ") (display history) (newline)
    (display "Frame: ") (display frame) (newline)
    (cond ((> count 50) ' failed)
          ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 history frame))
          ((var? p2) (extend-if-possible p2 p1 history frame))
          ((and (pair? p1)
                (pair? p2))
           (internal (cdr p1)
                     (cdr p2)
                     (extend-history p1 p2 history)
                     (internal (car p1)
                               (car p2)
                               (extend-history p1 p2 history)
                               frame)
                     ))
          (else 'failed)))
  (define (extend-if-possible var val history frame)
    (let ((binding (binding-in-frame var frame)))
      (cond (binding
             (internal
              (binding-value binding) val (extend-history var val history) frame))
            ((var? val)
             (let ((binding (binding-in-frame val frame)))
               (if binding
                   (internal
                    var (binding-value binding) (extend-history var val history) frame)
                   (extend var val frame))))
            ((depends-on? var val frame)
           'failed)
            (else (extend var val frame)))))
  (internal p1 p2 '() frame))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               #t
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     #f))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))

;; Retrieve assertions
(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

;; Retrieve rules
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append-delayed
   (get-stream (index-key-of pattern) 'rule-stream)
   (delay (get-stream '? 'rule-stream))))

;; Adding rules/assertions
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
          (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))
      (void)))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream))))
        (void))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;; Compound queries
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

;; Negate filter
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
     frame-stream))

;; Lisp value filter
(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate call
            frame
            (lambda (v f)
              (error "Unknown pat var -- LISP-VAL"))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp) ns)
         (args exp)))

;; Always true filter (no-op filter)
(define (always-true ignore frame-stream) frame-stream)

;; Install functionality into our evaluator
(put 'and 'qeval conjoin)
(put 'or 'qeval disjoin)
(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)
(put 'always-true 'qeval always-true)

;; Interpreter
(define (interpret program)
  (define (parse lines)
    (if (null? lines)
        (void)
        (let ((q (query-syntax-process (car lines))))
          (cond ((assertion-to-be-added? q)
                 (add-rule-or-assertion! (add-assertion-body q))
                 (newline)
                 (display "Assertion added to data base.")
                 (parse (cdr lines)))
                (else
                 (newline)
                 (display output-prompt)
                 (display-stream
                  (stream-map
                   (lambda (frame)
                     (instantiate q
                       frame
                       (lambda (v f)
                         (contract-question-mark v))))
                   (qeval q (singleton-stream '()))))
                 (parse (cdr lines)))))))
  (parse program))

(require "database.rkt")
(interpret (append assertions '((assert! (rule (outranked-by ?staff-person ?boss)
                                               (or (supervisor ?staff-person ?boss)
                                                  ; (and (outranked-by ?middle-manager ?boss)
                                                  ;      (supervisor ?staff-person ?middle-manager)))))
                                                   (and (supervisor ?staff-person ?middle-manager)
                                                        (outranked-by ?middle-manager ?boss)))))
                                (outranked-by (Bitdiddle Ben) ?who))))

(provide interpret)
(provide print-table)