( define ( cube n )
         ( * n n n ))

( define ( next x )
         ( + 1 x ))

( define ( sum term a next b)
         (define (iter a result)
           (if ( > a b )
             result
             (iter (next a) ( + ( term a ) result))))
         (iter a 0))
