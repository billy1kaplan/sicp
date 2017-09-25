(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))

( define ( reverse lst )
         ( define ( iter res lst )
                  ( if ( null? lst )
                       res
                       ( iter ( cons ( car lst ) res ) ( cdr lst ))))
         ( iter '() lst ))

( define ( deep-reverse lst )
         ( reverse ( map reverse lst )))

( define x ( list ( list 1 2 ) ( list 3 4 )))

( deep-reverse x )
