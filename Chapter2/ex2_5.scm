( define ( exp b n )
         ( define (iter result n )
             ( if ( = n 0 )
                  result
                  ( iter ( * b result ) ( - n 1 ))))
             ( iter 1 n ))

( define ( my-cons a b )
         ( * ( exp 2 a ) ( exp 3 b )))

( define ( find-val n d )
         ( define ( iter count )
                  ( if ( = ( remainder n ( exp d count )) 0 )
                       ( iter ( + count 1 ))
                       ( - count 1 )))
         ( iter 1 ))

( define ( my-car n )
         ( find-val n 2 ))

( define ( my-cdr n )
         ( find-val n 3 ))
