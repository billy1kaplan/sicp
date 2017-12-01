(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;63a 
; Procedure 1:
; (1 3 5 7 9 11)
; (1 3 5 7 9 11)
; (1 3 5 7 9 11)

; Procedure 2:
;Wrong, they are both in-order traversals
; (1 3 5 11 9 7)
; (1 5 11 9 7 3)
; (1 3 7 11 9 5)

;63b
;O(n) ~ n^2 (n log (n)), does half as much n steps each time
;O(n) ~ n
