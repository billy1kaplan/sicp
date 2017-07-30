( define tolerance 0.0001 )

( define ( cont-frac-iter n d k )
         ( define ( iter result count )
                  ( if ( = 0 count )
                       result
                       ( iter ( / ( n count ) ( + result ( d count ))) ( - count 1 ))))
         ( iter 0.0 k ))

( define ( frac k )
         ( cont-frac (lambda ( i ) 1.0)
                     (lambda ( i ) 1.0)
                     k ))

( define ( countsteps f target )
         ( define ( close-enough? v1 v2 )
                  ( < ( abs ( - v1 v2 ) ) tolerance ))
         ( define ( try val count )
                  ( let (( next ( f count )))
                        ( if ( close-enough? target val)
                             count
                             ( try next ( + count 1 )))))
         ( try 0 0))

( define ( square n )
         ( * n n ))

( define ( tan-cf x k )
         ( cont-frac-iter 
           ( lambda ( i )
                    ( if ( = i 1 )
                    x
                    ( - ( square x ))))
           ( lambda ( i )
                    ( + 1 ( * ( - i 1 ) 2 )))
           k))

( tan-cf 0.67 1000 )
