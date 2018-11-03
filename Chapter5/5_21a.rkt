#lang racket

(require "simulator.rkt")

(define (count-leaves)
  (make-machine
   '(tree val t continue)
   `((+ ,+) (null? ,null?) (pair? ,pair?)
            (car ,car) (cdr ,cdr))
   '((goto (label machine-start))

     count-leaves ;; Case depending on the type of the element considered
       (test (op null?) (reg tree))
       (branch (label leaf))
       (test (op pair?) (reg tree))
       (branch (label count))
       (goto (label single))

     count ;; A continuation of count-leaves
       (save continue) ;; Save current continuation
       (assign continue (label after-left)) ;; Set continuation (after next goto is after-left waiting for the result)
       (save tree) ;; Save the current tree
       (assign tree (op car) (reg tree)) ;; Count-leaves of the left-branch
       (goto (label count-leaves)) ;; Recursively count

     after-left ;; count of left is in val
       (restore tree) ;; Get the tree to set up right computation (this is the parent tree of this left-branch computation)
       (assign tree (op cdr) (reg tree)) ;; Set the tree 
       (assign continue (label after-right)) ;; Set the continuation. After after-left completes, after-right is waiting for the continuation of the computation
       (save val) ;; Save the result of counting the leaves on the left sub-tree
       (goto (label count-leaves)) ;; Count leaves on the right sub-tree
        
     after-right ;; val contains count-leaves of right sub-tree
       (restore t) ;; Restore the current count of the leaves
       (restore continue) ;; Restore the continuation
       (assign val (op +) (reg val) (reg t)) ;; The result is the sum of the leaves in the left sub-tree and right sub-tree (the answer)
       (goto (reg continue)) ;; Return computationo to caller
      
     machine-start ;; Entry point of machine
       (assign continue (label machine-end)) ;; The end of the machine is waiting for the result.
       (goto (label count-leaves)) ;; Begin computation

     leaf
       (assign val (const 0))
       (goto (reg continue))

     single
       (assign val (const 1))
       (goto (reg continue))

     machine-end)))

(define (test message expected machine-input)
  (let ((count-leaves-machine (count-leaves)))
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