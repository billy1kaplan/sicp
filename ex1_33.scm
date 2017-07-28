( define ( filtered-accumulate combiner null-value term a next b predicate )
         ( define ( iter a result )
                  ( cond (( > a b ) result )
                         (( predicate a ) (iter ( next a ) (combiner result ( term a ))))
                         ( else ( iter ( next a ) result ))))
         ( iter a null-value ))

( define ( square n )
         ( * n n ))

( define ( divides? n d )
         ( = ( remainder n d ) 0 ))

( define ( smalldiv n )
         ( helper 2 n ))

( define ( helper guess n )
         ( cond (( > ( square guess ) n ) n )
                (( divides? n guess ) guess )
                ( else ( helper ( + 1 guess ) n ))))

( define ( oddstart n )
         ( if ( even? n )
              ( + n 1 )
              n))

(define ( prime? n )
  ( if ( and ( = ( smalldiv n ) n ) ( not ( = n 1 )))
       #t
       #f ))

( define ( inc x )
         ( + x 1 ))

( define ( identity x )
         x)

( define ( ssqprimes a b )
         ( filtered-accumulate + 0 square a inc b prime? ))

( define ( GCD a b )
         ( if ( = b 0 )
              a
              ( GCD b ( remainder a b ))))

( define ( GCDpred a b )
         ( = ( GCD a b ) 1 ))

( define ( prodpos n )
         ( define ( filter x )
                  ( GCDpred x n ))
         ( filtered-accumulate * 1 identity 1 inc n filter ))

( ssqprimes 1 5 )
;( prodpos 10 ) 
