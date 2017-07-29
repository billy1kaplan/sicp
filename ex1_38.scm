( define tolerance 0.0001 )

( define ( cont-frac-iter n d k )
         ( define ( iter result count )
                  ( if ( = 0 count )
                       result
                       ( iter ( / ( n count ) ( + result ( d count ))) ( - count 1 ))))
         ( iter 0.0 k ))

( define ( frac k )
         ( cont-frac (lambda ( i ) 1.0)
                     (lambda ( i )
                       ( if ( = ( remainder ( - i 2 ) 3 ) 0 )
                            ( + 2 ( * 2 ( / i 3 )))
                            1
                            )
                       k )))

( define ( countsteps f target )
         ( define ( close-enough? v1 v2 )
                  ( < ( abs ( - v1 v2 ) ) tolerance ))
         ( define ( try val count )
                  ( let (( next ( f count )))
                        ( if ( close-enough? target val)
                             count
                             ( try next ( + count 1 )))))
         ( try 0 0))

;( countsteps frac ( / 1 1.61803398875 ))

( + ( cont-frac-iter (lambda ( i ) 1.0)
                 (lambda ( i )
                   ( if ( = ( remainder ( - i 2 ) 3 ) 0 )
                        ( + 2 ( * 2 ( / ( - i 2 ) 3 )))
                        1
                        ))
                 1000 ) 2 )

;((lambda ( i )
;  ( if ( = ( remainder ( - i 2 ) 3 ) 0 )
;       ( + 2 ( * 2 ( / ( - i 2 ) 3 )))
;       1
;       )) 2 )
