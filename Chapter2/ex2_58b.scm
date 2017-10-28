#lang sicp

(define (accumulate init op list)
  (if (null? list)
    init
    (op (car list) (accumulate init op (cdr list)))))

(define (is-op? expr)
  (cond ((null? expr) #f)
    ((or (eq? expr `+) (eq? expr `*)))
  (else #f)))

(define (min-precedence a b)
  (if (eq? `* b)
    a
    b))

(define (smallest-op expr)
  (accumulate '* (lambda (a b)
                   (if (is-op? a)
                     (min-precedence a b)
                     b))
              expr))

(define (=number? exp x)
  (and (number? exp)
       (= exp x)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (eq? '+ (smallest-op x)))

(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
    `()
    (cons (car list) (prefix sym (cdr list)))))

(define (addend s)
  (let ((a (prefix '+ s)))
    (if (pair? a)
        (if (eq? (cdr a) `())
            (car a)
            a)
        a)))

(define (augend s)
  (let ((a (cdr (memq '+ s))))
    (if (pair? a)
        (if (eq? (cdr a) `())
            (car a)
            a)
        a)))

(define (product? x)
  (eq? '* (smallest-op x)))

(define (multiplier p)
  (let ((a (prefix '* p)))
    (if (pair? a)
        (if (eq? (cdr a) `())
            (car a)
            a)
        a)))

(define (multicand p)
  (let ((a (cdr (memq '* p))))
    (if (pair? a)
        (if (eq? (cdr a) `())
            (car a)
            a)
        a)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) `**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

; Boiler plate above

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 `+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (=number? m1) (=number? m2)) (* m1 m2))
        (else (list m1 `* m2))))

(define (make-exponentation n x)
  (cond ((=number? n 1) 1)
        ((=number? x 0) 1)
        ((=number? x 1) n)
        (else (list '** n x))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multicand exp))))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((exponentiation? exp)
         (make-product (deriv (base exp) var)
                       (make-product (exponent exp)
                                     (make-exponentation (base exp)
                                                         (make-sum (exponent exp) -1)))))
        (else
          (error "unknown expresion type -- DERIV" exp))))

(deriv `(x + 3 * ( x + y + 2)) `x)
