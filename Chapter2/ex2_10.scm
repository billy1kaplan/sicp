( define ( make-interval a b ) ( cons a b ))

( define ( upperbound interval ) ( max  ( car interval ) ( cdr interval )))

( define ( lowerbound interval ) ( min  ( car interval ) ( cdr interval )))

( define ( subtract a b )
         ( make-interval
           ( - ( upperbound a ) ( lowerbound b ) )
           ( - ( lowerbound a ) ( upperbound b ) )))

( define ( mul-interval x y )
         ( let (( p1 ( * ( lower-bound x )
                         ( lower-bound y )))
                ( p2 ( * ( upper-bound x )
                         ( lower-bound y )))
                ( p3 ( * ( lower-bound x )
                         ( upper-bound y )))
                ( p4 ( * ( upper-bound x )
                         ( upper-bound y ))))
               ( make-interval ( min p1 p2 p3 p4 )
                               ( max p1 p2 p3 p4 ))))

( define ( div-interval x y )
         ( if ( = ( upper-bound y ) ( lower-bound y ))
              ( error "interval widthh is 0" )
              ( mul-interval x
                             ( make-inverval
                               ( / 1.0 ( upper-bound y ))
                               ( / 1.0 ( lower-bound y ))))))

( define x ( make-interval 2 5 ))
( define y ( make-interval 6 2 ))
( define z ( subtract x y ))
( newline )
( display ( upperbound z ))
( newline )
( display ( lowerbound z ))
( newline )
#t
