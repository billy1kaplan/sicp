( define ( make-point x y )
         ( cons x y ))

( define ( x-point point )
         ( car point ))

( define ( y-point point )
         ( cdr point ))

( define ( make-segment start-segment end-segment )
         ( cons start-segment end-segment ))

( define ( start-segment segment )
         ( car segment ))

( define ( end-segment segment )
         ( cdr segment ))

( define ( midpoint-segment segment )
         ( define ( average x y )
                  ( / ( + x y ) 2 ))
         ( make-point ( average ( x-point (start-segment segment)) ( x-point ( end-segment segment )))
                      ( average ( y-point (start-segment segment)) ( y-point ( end-segment segment )))))


( define ( print-point point )
         ( newline )
         ( display "(" )
         ( display ( x-point point ))
         ( display ", " )
         ( display ( y-point point ))
         ( display ")" )
         ( newline ))

( define segment ( make-segment ( make-point 10 10 ) ( make-point 20 30 )))
( print-point ( midpoint-segment segment ))
