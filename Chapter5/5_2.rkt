#lang racket

(require "simulator.rkt")
(define factorial-machine
  (make-machine
   '(a b c t)
   (list (list '> >) (list '* *) (list '+ +))
   '((assign b (const 1))
     (assign c (const 1))
     test-b
     (test (op >) (reg c) (reg a))
     (branch (label gcd-done))
     (assign t (op *) (reg b) (reg c))
     (assign b (reg t))
     (assign t (op +) (reg c) (const 1))
     (assign c (reg t))
     (goto (label test-b))
     gcd-done
     (assign a (reg b)))))

(set-register-contents! factorial-machine 'a 3)
(start factorial-machine)
(get-register-contents factorial-machine 'a) ;; Factorial of 3 => 6
(start factorial-machine)
(get-register-contents factorial-machine 'a) ;; Factorial of 6 => 720