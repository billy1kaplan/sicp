( define ( make-interval a b ) ( cons a b ))

( define ( upperbound interval ) ( max  ( car interval ) ( cdr interval )))

( define ( lowerbound interval ) ( min  ( car interval ) ( cdr interval )))

( define ( subtract a b )
         ( make-interval
           ( - ( upperbound a ) ( lowerbound b ) )
           ( - ( lowerbound a ) ( upperbound b ) )))

( define x ( make-interval 2 5 ))
( define y ( make-interval 6 2 ))
( define z ( subtract x y ))
( newline )
( display ( upperbound z ))
( newline )
( display ( lowerbound z ))
( newline )
#t
