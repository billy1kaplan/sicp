#lang racket

;4.12
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

(define (make-scan var eq-f empty-f)
  (define (scan vars vals)
    (cond [(null? vars)
           (empty-f)]
          [(eq? var (mcar vars))
           (eq-f vars vals)]
          [#t (scan (mcdr vars) (mcdr vals))]))
  (lambda (vars vals)
    (scan vars vals)))

(define (make-env-loop scan)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable")
        (let [(frame (first-frame env))]
          (scan (frame-variables frame)
                (frame-values frame)))))
  (lambda (env)
    (env-loop env)))

(define (lookup-abstract var env)
  (define (loop env)
    ((make-env-loop scan) env))
  (define (scan vars vals)
    ((make-scan var (lambda (vars vals) (mcar vals)) (lambda () (loop (enclosing-environment env)))) vars vals))
  (loop env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (mcar vars))
             (mcar vals)]
            [#t (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let [(frame (first-frame env))]
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-abstract! var val env)
  (define (loop env)
    ((make-env-loop scan) env))
  (define (scan vars vals)
    ((make-scan var (lambda (vars vals) (set-mcar! vals val)) (lambda () (loop (enclosing-environment env)))) vars vals))
  (loop env))

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

(define (define-abstract! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      ((make-scan var (lambda (vars vals) (set-mcar! vals val)) (lambda () (add-binding-to-frame! var val frame))) vars vals))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Acceptance tests
(define (mlist . args)
    (foldr mcons '() args))
(define e (mlist (mcons (mlist 'a 'b 'c) (mlist 'x 'y 'z))
                 (mcons (mlist 'd 'e 'f) (mlist 'x1 'y1 'z1))))
(eq? (lookup-variable-value 'a e) 'x)
(eq? (lookup-abstract 'a e) 'x)
(eq? (lookup-abstract 'c e) 'z)
(eq? (lookup-abstract 'e e) 'y1)
(set-abstract! 'e 'n e)
(eq? (lookup-abstract 'e e) 'n)
(define-abstract! 'a 'v e)
(eq? (lookup-abstract 'a e) 'v)
(define-abstract! 'd 'q e)
(eq? (lookup-abstract 'd e) 'q)
(eq? (lookup-abstract 'd (enclosing-environment e)) 'x1)