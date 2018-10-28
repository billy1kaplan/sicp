#lang racket

;; Implementing just the pattern matching portion of the 4.4
(define (var? exp)
  (not (or (number? exp)
           (pair? exp))))

(define the-empty-frame '())

(define (lookup-binding var frame)
  (cond ((null? frame) #f)
        ((equal? var (caar frame)) (cdar frame))
        (else (lookup-binding var (cdr frame)))))

(define (extend-frame var dat frame)
  (cons (cons var dat)
        frame))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (attempt-to-extend p1 p2 frame))
        ((var? p2) (attempt-to-extend p2 p1 frame))
        ((and (pair? p1)
              (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))


;; We attempt to extend our current frame:
;; If the binding for the variable is in the current frame, we fail if the existing binding does not match
;; the newest attempted binding (try to match the current pattern with the existing binding)
;; If the binding for the variable is not in the current frame, we can update the existing frame
(define (attempt-to-extend var val frame)
  (let ((binding (lookup-binding var frame)))
    (cond (binding
           (unify-match
            binding val frame))
          ((var? val)
           (let ((binding (lookup-binding val frame)))
             (if binding
                 (unify-match var
                              binding frame)
                 (extend-frame var val frame))))
          ((depends-on? var val frame)
           'failed)
          (else (extend-frame var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               #t
               (let ((b (lookup-binding e frame)))
                 (if b
                     (tree-walk b)
                     #f))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))

(unify-match '(x "a" y) '(y z "a") the-empty-frame)
(define test (unify-match '(x x) '(("a" y "c") ("a" "b" z)) the-empty-frame))
(lookup-binding 'z test)
