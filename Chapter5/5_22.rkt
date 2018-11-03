#lang racket

(require "simulator.rkt")

(define (append-machine)
  (make-machine
   '(list1 list2 t result continue)
   `((+ ,+) (null? ,null?) (pair? ,pair?) (cons ,cons) (car ,car) (cdr ,cdr))
   '((goto (label machine-start))

     append
       (test (op null?) (reg list1))
       (branch (label base-case))
       (save continue)
       (assign continue (label after-append))
       (save list1)
       (assign list1 (op cdr) (reg list1))
       (goto (label append))

     after-append
       (restore list1)
       (restore continue)
       (assign t (op car) (reg list1))
       (assign result (op cons) (reg t) (reg result))
       (goto (reg continue))

     base-case
       (assign result (reg list2))
       (goto (reg continue))

     machine-start
       (assign continue (label machine-end))
       (goto (label append))

     machine-end)))

(define (test message expected input1 input2)
  (let ((append-machine-local (append-machine))) ;; make a new machine for each test
    (set-register-contents! append-machine-local 'list1 input1)
    (set-register-contents! append-machine-local 'list2 input2)
    (start append-machine-local)
    (let ((actual (get-register-contents append-machine-local 'result)))
      (if (equal? expected actual)
          (display ".")
          (display (list "TEST FAILED: " message
                         "Expected: " expected
                         "received: " actual))))))

(test "Empty list" '() '() '())
(test "Empty first list" '(1 2 3) '() '(1 2 3))
(test "Empty second list" '(1 2 3) '(1 2 3) '())
(test "Non-empty lists" '(1 2 3 4 5 6) '(1 2 3) '(4 5 6))
