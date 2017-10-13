#lang sicp
(#%require sicp-pict)

(define (split orientation operation)
    (define (smaller painter n)
      (if (= n 0)
        painter
        (let ((tiny (smaller painter (- n 1))))
          (orientation painter (operation tiny tiny)))))
    smaller)

(define right-split
  (split beside below))

(define up-split
  (split below beside))

(paint (right-split einstein 5))

(paint (up-split einstein 5))
