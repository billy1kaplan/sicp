( define ( f n )
         ( if ( < n 3 )
              n
              ( + ( f ( - n 1 ) ) ( * 2 ( f ( - n 2 ))) ( * 3 ( f ( - n 3 ))))))

( define ( f1 n )
         ( fiter 2 1 0 n ))

( define ( fiter a b c n ) 
         ( if ( = n 0 )
              c
              ( fiter ( + ( * 3 c ) ( * 2 b ) a ) a b ( - n 1))))
