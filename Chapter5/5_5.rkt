#lang racket

(require "simulator.rkt")

(define factorial-rec
  (make-machine
    '(n retval retaddr x)
    `((= ,=) (+ ,+) (- ,-) (* ,*))
    '(
        (goto (label machine-start))

        ;;; procedure fact
      fact
        (test (op =) (reg n) (const 1))
        (branch (label fact-base))
        ; Push future work onto the stack:
        (save n)
        (save retaddr) ;; our first retaddr is the machine-end
                       ;; then it becomes fact-after-rec-return
        (assign retaddr (label fact-after-rec-return))
        (assign n (op -) (reg n) (const 1))
        (goto (label fact))

      fact-base
        (assign retval (const 1))
      fact-after-rec-return
        (restore retaddr)
        (restore n)
        (assign retval (op *) (reg retval) (reg n))
        (goto (reg retaddr))

      machine-start
        (assign retaddr (label machine-end))
        (goto (label fact))

      machine-end
    )))

(set-register-contents! factorial-rec 'n 3)
(start factorial-rec)
(get-register-contents factorial-rec 'retval)

;; stack: (3 2 1) <= order of insertion (pushing and popping at the left-most end)

;; Simulation of Factorial:
;; Start:
;; Set: n = 3

;; goto machine-start
;; n = 3
;; stack = ()
;; retaddr = machine-end

;; goto fact:
;; n = 3
;; start: stack = ()
;; end: stack   = ((label machine-end) 3)
;; retaddr = fact-after-rec-return

;; goto fact:
;; n = 2
;; start: stack = ((label machine-end) 3)
;; end: stack   = ((label fact-after-rec-return) 2 
;;                 (label machine-end) 3)

;; fact:
;; n = 1
;; goto fact-base:
;; retval = 1
;; fact-end:
;; stack = ((label fact-after-rec-return) 2
;;          (label machine-end) 3)

;; roll into (label fact-after-rec-return)
;; pop n = 2
;; pop retaddr = fact-after-rec-return
;; retval = (* n retval) 
;;        = (* 2 1)
;;        = 2
;; start: stack = ((label fact-after-rec-return) 2
;;                 (label machine-end) 3)
;; end:   stack = ((label machine-end) 3)

;; goto (label fact-after-rec-return)
;; pop n = 3
;; pop retaddr = fact-after-rec-return
;; retval = (* n retval) 
;;        = (* 3 2)
;;        = 6
;; start: stack = ((label machine-end) 3)
;; end:   stack = ()

;; goto (label machine-end)
;; retval = 6

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
      (restore retaddr)
      (assign n (op -) (reg n) (const 2))
      (save retaddr)
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

;; Start, set n = 3
;; retaddr = fib-end

;; fib:
;; n = 3
;; Start: stack = ()
;; End:   stack = (3 fib-end)
;; retaddr = after-fib-1

;; fib:
;; n = 2
;; Start: stack = (3 fib-end)
;; End:   stack = (2 after-fib-1 3 fib-end)

;; fib:
;; n = 1
;; Test goes to fib-base:
;; retval = 1

;; afterfib-n-1:
;; pop n = 2
;; n = 0
;; retaddr = after-fib-2
;; Start: stack = (2 after-fib-1 3 fib-end)
;; End:   stack = (1 after-fib-1 3 fib-end)

;; fib:
;; n = 0
;; Test goes to fib-base:
;; retval = 0

;; afterfib-n-2:
;; n = 0
;; pop retval = 1
;; pop retaddr = after-fib-1
;; retval = 1
;; Start: stack = (2 after-fib-1 3 fib-end)
;; End:   stack = (3 fib-end)

;; afterfib-n-1:
;; pop n = 3
;; n = 1
;; Start: stack = (3 fib-end)
;; End:   stack = (1 fib-end)

;; fib:
;; n = 1
;; Test goes to fib-base:
;; retval = 1

;; afterfib-n-2:
;; n = 1
;; pop retval = 1
;; pop retaddr = fib-end
;; retval = 2
;; Start: stack = (1 fib-end)
;; End:   stack = ()

;; done, retval = 2
