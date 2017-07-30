;( define ( * a b )
;         ( if ( = b 0 )
;              0
;              ( + a ( * a ( - b 1 )))))

( define ( double n )
         ( + n n ))

( define ( halve n )
         ( / n 2 ))

( define ( even? n )
         ( = ( remainder n 2 ) 0 ))

( define ( * a b )
         ( mult 0 a b ))

( define ( mult res a b )
         ( cond (( = b 0 ) res )
                (( even? b ) ( mult res ( double a ) ( / b 2 )))
                ( else ( mult ( + res a ) a ( - b 1 )))))
