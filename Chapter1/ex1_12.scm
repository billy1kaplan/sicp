( define ( pascal r n )
         ( cond (( or (= n r ) ( = n 1 ) ) 1 )
                (( or ( < n 1 ) ( > n r ) ) 0 )
                (( + ( pascal ( - r 1 ) ( - n 1 ) ) ( pascal ( - r 1 ) n )))))
