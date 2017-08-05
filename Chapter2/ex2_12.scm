( define ( make-interval a b ) ( cons a b ))

( define ( upperbound interval ) ( max  ( car interval ) ( cdr interval )))

( define ( lowerbound interval ) ( min  ( car interval ) ( cdr interval )))

( define ( mul-interval x y )
         ( define ( positive? int )
                  ( and ( >= 0 ( lowerbound int ) ) ( >= 0 ( upperbound int ))))
         ( define ( opposite? int )
                  ( and ( < ( lowerbound int ) 0 ) ( >= 0 ( upperbound int ))))
         ( define ( negative? int )
                  ( and ( < ( lowerbound int ) 0 ) ( < ( upperbound int ) 0 )))
         ( let (( lx ( lowerbound x  ))
                ( ux ( upperbound x  ))
                ( ly ( lowerbound y  ))
                ( uy ( lowerbound y  )))
               ( cond (( ( and ( positive? x ) ( positive? y )) ( make-interval ( * lx ly ) ( * ux uy )))
                       ( ( and ( positive? x ) ( opposite? y )) ( make-interval ( * ux ly ) ( * ux uy )))
                       ( ( and ( positive? x ) ( negative? y )) ( make-interval ( * ux ly ) ( * lx ly )))
                       ( ( and ( negative? x ) ( opposite? y )) ( make-interval ( * lx uy ) ( * lx ly )))
                       ( ( and ( negative? x ) ( negative? y )) ( make-interval ( * lx ly ) ( * ux uy )))
                       ( ( and ( opposite? x ) ( opposite? y )) ( make-interval ( min ( * lx uy ) ( * ux ly )) ( max ( * lx ly ) ( * ux uy ))))
                       ; for efficiency, would probably write out the the other 4 cases
                       ( else ( mul-interval y x ))))))

( define ( make-center-toler c percent )
         ( let (( p ( /  percent 100.0 )))
         ( make-interval ( - c ( * c p )) ( + c ( * c p )))))

( define ( center x )
         ( / ( + ( lowerbound x ) ( upperbound x )) 2.0 ))

( define ( width int )
         ( - (upperbound int ) ( center int )))

( define ( tolerance int )
         ( * ( / ( width int ) ( center int) ) 100 ))

( define a ( make-center-toler 10 1 ))
( define b ( make-center-toler 100 2 ))
( tolerance x )
