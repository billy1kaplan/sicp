( define tolerance 0.000001 )

( define ( fixed-point f first-guess )
         ( define ( close-enough? v1 v2 )
                  ( < ( abs ( - v1 v2 ) ) tolerance ))
         ( define ( try guess )
                  ( let (( next ( f guess )))
                        ( display guess )
                        ( newline )
                        ( if ( close-enough? guess next)
                             next
                             ( try next ))))
         ( try first-guess ))

;( fixed-point ( lambda ( x ) ( / ( log 1000 ) ( log x ))) 1.5 )
( fixed-point ( lambda ( x ) ( + ( / ( log 1000 ) ( * 2 ( log x ))) (/ x 2 ))) 1.5 )
