( define tolerance 0.0001 )

( define (cont-frac n d k )
         ( define ( frac count )
                  ( let (( num ( n count ))
                         ( den ( d count )))
                        ( if ( = count k )
                             0
                             ( / num ( + den ( frac ( + count 1 )))))))
         ( frac 0 ))

;( define ( cont-frac-iter n d k )
;         ( define ( iter num den num1 den1 num2 den2 count )
;                  ( let (( newn ( n count ))
;                         ( newd ( d count )))
;                        if ( = count k )
;                        ( / num den )
;                        ( iter ( + ( * num newn ) num2 ) ( + ( * den newd ) den2 ) num den num1 den1 ( + count 1 ))))
;         ( iter 1 1 0 ( n 2 ) 0 0 0 ))

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

;( countsteps frac ( / 1 1.61803398875 ))

( cont-frac-iter (lambda ( i ) 1.0)
            (lambda ( i ) 1.0)
            1000 )
