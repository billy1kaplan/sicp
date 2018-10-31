#lang racket

(require "simulator.rkt")

(define fib-rec
  (make-machine
    '(n retval retaddr x)
    `((< ,<) (+ ,+) (- ,-))
    '((assign retaddr (label fib-end))
      fib
      (test (op <) (reg n) (const 2))
      (branch (label fib-base))
      (save retaddr)
      (save n)
      (assign retaddr (label after-fib-1))
      (assign n (op -) (reg n) (const 1))
      (goto (label fib))

      after-fib-1
      (restore n)
      ;(restore retaddr) <= retaddr can never change in between. The benefit of pushing the value after the continuation (we can access value w/o going through)
      (assign n (op -) (reg n) (const 2))
      ;(save retaddr) <= retaddr thrown away on the next line, so not used
      (assign retaddr (label afterfib-n-2))
      (save retval)
      (goto (label fib))

      afterfib-n-2
      (assign n (reg retval))
      (restore retval)
      (restore retaddr)
      (assign retval
              (op +) (reg retval) (reg n))
      (goto (reg retaddr))

      fib-base
      (assign retval (reg n))
      (goto (reg retaddr))
      
      fib-end)))

(set-register-contents! fib-rec 'n 5)
(start fib-rec)
(get-register-contents fib-rec 'retval)
