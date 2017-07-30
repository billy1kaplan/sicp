;copied, need to revisit

( define ( square n )
         ( * n n))

( define ( miller-rabin n )
         ( miller-rabin-test ( - n 1 ) n ))

( define ( miller-rabin-test a n )
         ( cond (( = a 0 ) #t )
                (( = ( expmod a ( - n 1 ) n ) 1) miller-rabin-test ( - a 1 ) n )
                ( else #f )))

( define ( expmod base exp  m )
         ( cond (( = exp 0 ) 1)
                (( even? exp )
                 ( let (( x ( expmod base ( / exp 2 ) m )))
                       ( if ( non-trivial-sqrt? x m )
                            0
                            ( remainder ( square x ) m ))))
                ( else ( remainder ( * base ( expmod ( base ( - exp 1 ) m ))) m))))

( define ( non-trivial-sqrt? n m )
         ( cond (( = n 1 ) #f )
                (( = n ( - m 1 ) #f ))
                ( else ( = ( remainder ( square n ) m ) 1 ))))
