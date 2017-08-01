( define ( make-point x y )
         ( cons x y ))

( define ( x-point point )
         ( car point ))

( define ( y-point point )
         ( cdr point ))

( define ( make-rectangle top-left bot-right )
         ( define ( min v1 v2 ) 
                  ( if ( < v1 v2 )
                       v1
                       v2 ))
         ( define ( max v1 v2 ) 
                  ( if ( > v1 v2 )
                       v1
                       v2 ))
         ( let ( ( x1 (x-point top-left))
                ( x2 (x-point bot-right))
                ( y1 (y-point top-left))
                ( y2 (y-point bot-right)))
               ( cons ( make-point (min x1 x2 ) (max y1 y2))
                      ( make-point (max x1 x2) (min y1 y2)))))

( define ( top-left rectangle )
         ( car rectangle ))

( define ( bot-right rectangle )
         ( cdr rectangle ))

( define ( width rectangle )
         ( - ( x-point ( bot-right rectangle) ) ( x-point ( top-left rectangle))))

( define ( length rectangle )
         ( - ( y-point ( top-left rectangle) ) ( y-point ( bot-right rectangle))))

( define ( make-rectangle1 top-left length height )
         ( cons top-left ( cons length height )))

( define ( position rectangle )
         ( car rectangle ))

( define ( length rectangle )
         ( display "HERE" )
         ( cdr ( car rectangle )))

( define ( width rectangle )
         ( cdr ( cdr rectangle )))

( define ( area rectangle )
         ( * ( length rectangle )
             ( width rectangle )))

( define ( perimeter rectangle )
         ( * 2 
             ( + ( length rectangle )
                 ( width rectangle ))))

( define ( print-point point )
         ( newline )
         ( display "(" )
         ( display ( x-point point ))
         ( display ", " )
         ( display ( y-point point ))
         ( display ")" )
         ( newline ))

( define r (make-rectangle ( make-point 10 10 ) ( make-point 20 30 )))
( define r1 (make-rectangle1 ( make-point 10 10 ) 10 20 ))
         ( display "HERE" )
( perimeter r1 )
