#lang racket
(require "simulator-stack-trace.rkt")

(define factorial-machine
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
        (assign retaddr (op initialize-stack))
        (assign retaddr (label machine-end))
        (goto (label fact))

      machine-end
        (assign x (op print-stack-statistics))
    )))



;; Testing:
(define (run-machine n)
  (newline)
  (display (list "Running machine: " n))
  (set-register-contents! factorial-machine 'n n)
   (start factorial-machine))
   ;(factorial-machine 'print-statistics)))

(define (int-range n)
  (define (iter cur)
    (if (> cur n)
        '()
        (cons cur
              (iter (+ cur 1)))))
  (iter 2))

(for-each (lambda (n)
            (run-machine n))
          (int-range 10))

;; Ops:
;; (Running machine:  2)
;; (total-pushes 2 maximum-depth 2)
;; (Running machine:  3)
;; (total-pushes 4 maximum-depth 4)
;; (Running machine:  4)
;; (total-pushes 6 maximum-depth 6)
;; (Running machine:  5)
;; (total-pushes 8 maximum-depth 8)
;; (Running machine:  6)
;; (total-pushes 10 maximum-depth 10)
;; (Running machine:  7)
;; (total-pushes 12 maximum-depth 12)
;; (Running machine:  8)
;; (total-pushes 14 maximum-depth 14)
;; (Running machine:  9)
;; (total-pushes 16 maximum-depth 16)
;; (Running machine:  10)
;; (total-pushes 18 maximum-depth 18)

;; Max depth and total pushes:
;; d(n) = 2*n - 2
