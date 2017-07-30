( define ( exp b n )
         ( fast-expt 1 b n ))

( define ( even? n )
         ( = ( remainder n 2 ) 0 ))

( define ( square n )
         ( * n n ))

( define ( fast-expt a b n )
         ( cond (( = n 0 ) a)
                (( even? n ) ( fast-expt a ( square b ) ( / n 2 )))
                ( else ( fast-expt ( * a b ) b ( - n 1 )))))
