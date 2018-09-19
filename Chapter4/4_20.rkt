#lang racket
; PART A

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define letrec-bindings cadr)
(define letrec-body cddr)

;; input : '(letrec ((<var1> <expr1>) (<var2> <expr2>) ... (<varn> <exprn>)) <expr-body>)
(define (transform-letrec letrec-expr)
  ;; ((var . val) ...)
  (define (transform-to-lets unassigned assigned bindings)
    (if (null? bindings)
        (append (list 'let (reverse unassigned)) (reverse assigned))
        (transform-to-lets (cons (list (caar bindings) ''*unassigned*) unassigned)
                           (cons (list 'set! (caar bindings) (cadar bindings)) assigned)
                           (cdr bindings))))
  (define (transform bindings body)
    (append (transform-to-lets '() '() bindings) body))
  (transform (letrec-bindings letrec-expr) (letrec-body letrec-expr)))


(define test-input '(letrec ((even?
                              (lambda (n)
                                (if (= n 0)
                                    true
                                    (odd? (- n 1)))))
                             (odd?
                              (lambda (n)
                                (if (= n 0)
                                    false
                                    (even? (- n 1))))))
                      (even? 2)))

(define test-output '(let ((even? '*unassigned*)
                           (odd? '*unassigned*))
                       (set! even?
                             (lambda (n)
                               (if (= n 0)
                                   true
                                   (odd? (- n 1)))))
                       (set! odd?
                             (lambda (n)
                               (if (= n 0)
                                   false
                                   (even? (- n 1)))))
                       (even? 2)))

(equal? test-output (transform-letrec test-input))


; PART B
; See notebook for env drawings

; This fails during the evaluation of the bindings for let. Since our environment does not lambda environment does not contain the binding for odd?, this will fail with a reference to an identifier before definition