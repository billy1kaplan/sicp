#lang racket
(require "simulator.rkt")


;; Assuming we have very useful primitives...
(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (average n1 n2)
  (/ (+ n1 n2) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define sqrt-machine
  (make-machine
   '(a guess t)
   (list (list 'good-enough? good-enough?) (list 'improve improve))
   '((assign guess (const 1.0))
     test-b
     (test (op good-enough?) (reg guess) (reg a))
     (branch (label iter-done))
     (assign t (op improve) (reg guess) (reg a))
     (assign guess (reg t))
     (goto (label test-b))
     iter-done
     (assign a (reg guess)))))

(display "Start: ")
(newline)
(set-register-contents! sqrt-machine 'a 4)
(start sqrt-machine)
(get-register-contents sqrt-machine 'a)

(newline)
(display "Using hardware primitives: ")
(newline)

(define sqrt-machine-fewer-primitives
  (make-machine
   '(a guess t)
   (list (list '- -)
         (list '/ /)
         (list '* *)
         (list '+ +)
         (list '< <))
   '((assign guess (const 1.0))
     test-b
     (assign t (op *) (reg guess) (reg guess))
     (assign t (op -) (reg t) (reg a))
     (test (op <) (const 0) (reg t))
     (branch (label gt))
     (assign t (op *) (reg t) (const -1))
     gt
     (test (op <) (reg t) (const 0.0001))
     (branch (label iter-done))
     (assign t (op /) (reg a) (reg guess))
     (assign t (op +) (reg t) (reg guess))
     (assign t (op /) (reg t) (const 2))
     (assign guess (reg t))
     (goto (label test-b))
     iter-done
     (assign a (reg guess)))))

(set-register-contents! sqrt-machine-fewer-primitives 'a 4)
(start sqrt-machine-fewer-primitives)
(get-register-contents sqrt-machine-fewer-primitives 'a)