#lang racket

(require "simulator.rkt")

(define (count-leaves-iter)
  (make-machine
   '(tree val t continue)
   `((+ ,+) (null? ,null?) (pair? ,pair?) (car ,car) (cdr ,cdr))
   '((goto (label machine-start))

     count-leaves
       (test (op null?) (reg tree))
       (branch (label leaf))
       (test (op pair?) (reg tree))
       (branch (label count))
       (goto (label atom))

     count
       (save continue)
       (save tree)
       (assign continue (label after-count))
       (assign tree (op car) (reg tree))
       (goto (label count-leaves))

     after-count
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (goto (label count-leaves))

     leaf
       (goto (reg continue))

     atom
       (assign val (op +) (reg val) (const 1))
       (goto (reg continue))

     machine-start
       (assign continue (label machine-end))
       (assign val (const 0))
       (goto (label count-leaves))

     machine-end)))

(define (test message expected machine-input)
  (let ((count-leaves-machine (count-leaves-iter))) ;; make a new machine for each test
    (set-register-contents! count-leaves-machine 'tree machine-input)
    (start count-leaves-machine)
    (let ((actual (get-register-contents count-leaves-machine 'val)))
      (if (equal? expected actual)
          (display ".")
          (display (list "TEST FAILED: " message
                         "Expected: " expected
                         "received: " actual))))))

(test "Empty tree" 0 '())
(test "Atomic tree" 1 '0)
(test "Simple tree" 2 '(1 2))
(test "Complex tree" 5 '(((1 2) 3) (4 (5))))