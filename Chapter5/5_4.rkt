#lang racket
(require "simulator.rkt")

(define rec-expt
  (make-machine
   '(b n t retval retaddr)
   (list (list '= =) (list '* *) (list '- -))
   '((save n)
     (assign retaddr (label machine-end))
     (save retaddr)

     expt
     (restore retaddr)
     (restore n)
     (save n)
     (save retaddr)
     (test (op =) (reg n) (const 0))
     (branch (label expt-base))
     (assign t (op -) (reg n) (const 1))
     (save t)
     (assign retaddr (label expt-after-rec-return))
     (save retaddr)
     (goto (label expt))

     expt-after-rec-return
     (assign retval (op *) (reg retval) (reg b))
     (goto (label expt-end))

     expt-base
     (assign retval (const 1))

     expt-end
     ;; Restore the caller's registers
     (restore retaddr)
     (restore n)
     (goto (reg retaddr)) ;; Return to caller
     
      machine-end)))

(set-register-contents! rec-expt 'b 2)
(set-register-contents! rec-expt 'n 5)
(start rec-expt)
(get-register-contents rec-expt 'retval)

(newline)
(display "Iterative Definition")
(newline)

(define iter-expt
  (make-machine
   '(b n retval)
   (list (list '= =) (list '* *) (list '- -))
   '((assign retval (const 1))

     expt
     (test (op =) (reg n) (const 0))
     (branch (label expt-end))
     (assign n (op -) (reg n) (const 1))
     (assign retval (op *) (reg b) (reg retval))
     (goto (label expt))
     expt-end)))

(set-register-contents! iter-expt 'b 2)
(set-register-contents! iter-expt 'n 5)
(start iter-expt)
(get-register-contents iter-expt 'retval)