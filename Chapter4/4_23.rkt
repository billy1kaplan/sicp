#lang racket

; Transcribed from Alyssa P. Hacker
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

; Original proposed function
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (loop (car procs) (cdr procs)))))

; Example function (begin (+ 2 2))
; For the original proposed function:
; (begin (+ 2 2)) -> (primitive + 2 2)
; (begin (+ 2 2) (+ 3 3)) -> (lambda (env) ((primitive + 2 2) env) ((primitive + 3 3)) env)

; For Alyssa's function: Must perform additional work at runtime:
; (begin (+ 2 2)) -> (lambda (env) (execute-sequence (primitive + 2 2)) env))
; (begin (+ 2 2) (+ 3 3)) -> (lambda (env) (execute-sequence ((primitive + 2 2) (primitive + 3 3)) env))