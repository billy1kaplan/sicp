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
  (cons (cons var dat) frame))

(define (pattern-match pat dat frame)
  ;; Pattern matching is a case analysis

  ;; Fail if our frame is 'failed
  ;; Ensures if any of our match fails, we will fail
  ;; e.g. (a b b) (1 2 3)
  ;; Once we go to the second b, we attempt to extend and fail.
  ;; This fails the entire frame and the pattern match fails
  (cond ((eq? frame 'failed) 'failed)

        ;; If our pattern and data are the same, the match succeeds with whatever bindings are in the current frame
        ((equal? pat dat) frame)

        ;; If we are attempting to match a variable, we attempt to extend our current frame
        ((var? pat)
         (attempt-to-extend pat dat frame))

        ;; If both our pattern and data to match are pairs, then we should match the rest of the pair in the frame
        ;; produced by matching the first element in the list. Of course, if the match fails, everything fails.
        ;; Otherwise, we may have some bindings in our frame with which we want to ensure consistency for pattern matching.
        ((and (pair? pat)
              (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match
                         (car pat)
                         (car dat)
                         frame)))

        ;; Any other case is a match failure
        (else 'failed)))


;; We attempt to extend our current frame:
;; If the binding for the variable is in the current frame, we fail if the existing binding does not match
;; the newest attempted binding (try to match the current pattern with the existing binding)
;; If the binding for the variable is not in the current frame, we can update the existing frame
(define (attempt-to-extend var dat frame)
  (let ((binding (lookup-binding var frame)))
    (if binding
        (pattern-match binding dat frame)
        (extend-frame var dat frame))))

"Should Succeed"
(pattern-match '((a b) b c c) '((1 2) 2 (4 5 6) (4 5 6)) the-empty-frame)
(pattern-match '(1 2 3 a b c) '(1 2 3 4 5 6) the-empty-frame)
(pattern-match '(a (b (c 1)) c) '((1 2 3) (1 (2 1)) 2) the-empty-frame) 
(newline)
"Should Start to Fail"
(pattern-match '((a b) b c) '((1 2) 3 (4 5 6)) the-empty-frame)
(pattern-match 'a '1 (extend-frame 'a '2 the-empty-frame))