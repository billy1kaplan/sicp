( define ( square n )
         ( * n n ))

( define ( divides? n d )
         ( = ( remainder n d ) 0 ))

( define ( smalldiv n )
         ( helper 2 n ))

( define ( helper guess n )
         ( cond (( > ( square guess ) n ) n)
                (( divides? n guess ) guess )
                ( else ( helper ( + guess 1 ) n ))))
