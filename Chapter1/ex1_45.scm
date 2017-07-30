( define dx 0.0001 )

( define ( compose f g )
         ( lambda (x)
                  ( f ( g x ))))

( define ( repeated f n )
              ( if ( = n 1 )
                   f
                   ( compose ( repeated f ( - n 1 ) ) f )))

( define ( square x )
         ( * x x ))

( define ( inc x )
         ( + x 1 ))

( define ( smooth f )
         ( lambda (x) ( / ( + ( f x ) ( f ( - x dx ) ( f ( + x dx )))) 3 )))

( define ( nsmooth f n )
         ( ( repeated smooth n ) f ))

( define tolerance 0.00000000000001 )

( define ( exp x n )
         ( if ( = n 0 )
              1
              ( * x ( exp x ( - n 1 )))))

( define ( fixed-point f first-guess )
         ( define ( close-enough? v1 v2 )
                  ( < ( abs ( - v1 v2 ) ) tolerance ))
         ( define ( try guess )
                  ( let (( next ( f guess )))
                        ( if ( close-enough? guess next)
                             next
                             ( try next ))))
         ( try first-guess ))

( define ( average-damp f )
         ( lambda ( x )
                  ( / ( + x ( f x )) 2 )))

( define ( calc n )
         ( floor ( / (log n ) ( log 2 ) )))

( define ( nthroot x n )
         ( let (( p ( floor ( / (log n ) ( log 2 )))))
         ( fixed-point 
           (( repeated average-damp p ) ( lambda ( y ) ( / x ( exp y ( - n 1 )))))
           1.0)))

( nthroot 7 124 )
