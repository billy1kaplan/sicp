#lang racket

; 4.16
;a
(define (enclosing-environment env) (mcdr env))

(define (first-frame env) (mcar env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame)
  (mcar frame))

(define (frame-values frame)
  (mcdr frame))

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
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(and (eq? var (mcar vars))
                  (eq? '*unassigned* (mcar vals)))
             (error "Found unassigned variable bound to " var)]
            [(eq? var (mcar vars))
             (mcar vals)]
            [#t (scan (mcdr vars) (mcdr vals))]))
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
            [#t (scan (mcdr vars) (mcdr vals))]))
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
            [(eq? var (mcar vars))
             (set-mcar! vals val)]
            [#t (scan (mcdr vars) (mcdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))


;; Acceptance tests
(define (mlist . args)
    (foldr mcons '() args))
(define e (mlist (mcons (mlist 'a 'b 'c) (mlist 'x 'y '*unassigned*))
                 (mcons (mlist 'd 'e 'f) (mlist 'x1 'y1 'z1))))
(eq? (lookup-variable-value 'e e) 'y1)
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (lookup-variable-value 'c e))

;b
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cadr exp)
                   (cddr exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (first-binding exp) (car exp))
(define (let-var binding) (car binding))
(define (let-binding binding) (cadr binding))
(define (rest-bindings exp) (cdr exp))
(define (let-bindings exp)
  (car exp))
(define (let-body exp)
  (list (cadr exp)))

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
  (let-builder '() '() '() exp))

(define (zip . args)
  (if (null? (caar args))
      '()
      (cons (map car args)
            (zip (map cdr args)))))
  
(define (make-let vars vals body)
  (let [(bindings (zip vars vals))]
    (list 'let bindings body)))

(define (scan-out-defines proc-body)
  ;; (cons <define free body> <rest of body>)
  (define (scan-out-internal body define-accum rest-accum)
    (if (null? body)
        (cons (reverse define-accum) (reverse rest-accum))
        (let ((cur (car body))
              (rest (cdr body)))
          (if (definition? cur)
              (scan-out-internal rest (cons cur define-accum) rest-accum)
              (scan-out-internal rest define-accum (cons cur rest-accum))))))
  (define (transform-to-lets unassigned assigned definitions)
    (if (null? definitions)
        (append (list 'let (reverse unassigned)) (reverse assigned))
        (transform-to-lets (cons (list (definition-variable (car definitions)) ''*unassigned*) unassigned)
                           (cons (list 'set! (definition-variable (car definitions)) (definition-value (car definitions))) assigned)
                           (cdr definitions))))
  (define (transform scanned-body)
    (append (transform-to-lets '() '() (car scanned-body)) (cdr scanned-body)))
  (if (or (null? proc-body) (not (pair? proc-body)))
      proc-body
      (transform (scan-out-internal proc-body '() '()))))

;; Test
(define proc '((define u 1)
               (display 1)
               (define v 2)
                3))

(define expected-scanned '(let ((u '*unassigned*)
                                (v '*unassigned*))
                            (set! u 1)
                            (set! v 2)
                            (display 1)
                            3))
(equal? expected-scanned (scan-out-defines proc))

;; 4.16c
;; I think it should go in procedure body. We should delay this expansion until the last "responsible" moment. Also, this allows us to perform other kinds of expansions and determine the order later on without coupling the
;; data structure to this transform.

;; Alternatively, we would only call this once if installed into make-procedure which is probably preferable from both a simplicity and performance standpoint.