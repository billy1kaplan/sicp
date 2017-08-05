( define ( reverse lst )
         ( define ( iter lst res )
                  ( if ( null? lst )
                       res
                       ( iter ( cdr lst ) ( cons ( car lst ) res ))
                       )
                  )
         ( iter lst () )
         )

( reverse ( list 1 2 3 4 5 ))
