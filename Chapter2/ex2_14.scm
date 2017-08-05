( define ( make-interval a b ) ( cons a b ))

( define ( upperbound interval ) ( max  ( car interval ) ( cdr interval )))

( define ( lowerbound interval ) ( min  ( car interval ) ( cdr interval )))

( define ( subtract a b )
         ( make-interval
           ( - ( upperbound a ) ( lowerbound b ) )
           ( - ( lowerbound a ) ( upperbound b ) )))

( define ( mul-interval x y ) 
         ( let (( p1 ( * ( lowerbound x )
                         ( lowerbound y )))
                ( p2 ( * ( upperbound x )
                         ( lowerbound y )))
                ( p3 ( * ( lowerbound x )
                         ( upperbound y )))
                ( p4 ( * ( upperbound x )
                         ( upperbound y ))))
               ( make-interval ( min p1 p2 p3 p4 )
                               ( max p1 p2 p3 p4 ))))

( define ( div-interval x y )
         ( if ( = ( upperbound y ) ( lowerbound y ))
              ( error "interval width is 0" )
              ( mul-interval x
                             ( make-interval
                               ( / 1.0 ( upperbound y ))
                               ( / 1.0 ( lowerbound y ))))))

( define ( make-center-toler c percent )
         ( let (( p ( /  percent 100.0 )))
               ( make-interval ( - c ( * c p )) ( + c ( * c p )))))

( define ( center x )
         ( / ( + ( lowerbound x ) ( upperbound x )) 2.0 ))

( define ( width int )
         ( - (upperbound int ) ( center int )))

( define ( tolerance int )
         ( * ( / ( width int ) ( center int) ) 100 ))

( define ( print-interval i )
         ( newline )
         ( display ( lowerbound i ))
         ( newline )
         ( display ( upperbound i ))
         ( newline ))

( define a ( make-center-toler 10000 1 ))
( define b ( make-center-toler 1000 2 ))

( print-interval ( div-interval a a ))
