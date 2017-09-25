(define (square-tree tree)
  ( define ( square x ) (* x x))
  (cond ((null? tree) `())
        ((not (pair? tree))
         ( square tree ))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  ( define ( square x ) (* x x))
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
            (square-tree-map sub-tree)
            (square sub-tree)))
        tree))

( let (( x (square-tree
             (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7))))
       ( y (square-tree-map
             (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))))
      ( equal? x y ))
