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
