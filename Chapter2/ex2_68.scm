#lang sicp

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)
          (append (cdr list1) list2))))

; Leaf functions
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;Tree selectors
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; Code Tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; Decode stuff
(define (choose-branch bit branch)
  (cond ((= 0 bit) (left-branch branch))
        ((= 1 bit) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
; Decodes to (A D A B B C A)

(define (encode message tree)
  (if  (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (contains el list)
  (cond ((null? list) #f)
        ((eq? (car list) el))
        (else (contains el (cdr list)))))

(define (encode-symbol symbol tree)
  (define (encode-the-symbol symbol tree)
    (cond ((null? tree) '())
          ((leaf? tree) '())
          ((contains symbol (symbols (left-branch tree)))
           (cons 0
                 (encode-the-symbol symbol (left-branch tree))))
          ((contains symbol (symbols (right-branch tree)))
           (cons 1
                 (encode-the-symbol symbol (right-branch tree))))))
  (if (contains symbol (symbols tree))
    (encode-the-symbol symbol tree)
    (error "bad symbol -- ENCODE-SYMBOL" symbol)))


(define encoding
  (encode '(A D A B B C A) sample-tree))
encoding

(decode encoding sample-tree)