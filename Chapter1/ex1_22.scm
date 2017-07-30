( define ( timed-prime-test3 n )
         (newline)
         (display n)
         (start-prime-test (oddstart n) 3 (runtime)))

(define (start-prime-test n count start-time)
  ( cond (( = count 0 ) (report-prime n ( - (runtime) start-time)))
         ((prime? n) (start-prime-test ( + n 2 ) ( - count 1 ) start-time ))
         ( else ( start-prime-test ( + n 2 ) count start-time ))))

(define (report-prime n elapsed-time)
  (newline)
  (display n )
  (newline)
  elapsed-time)

( define ( square n )
         ( * n n ))

( define ( divides? n d )
         ( = ( remainder n d ) 0 ))

( define ( smalldiv n )
         ( helper 2 n ))

( define ( helper guess n )
         ( cond (( > ( square guess ) n ) n )
                (( divides? n guess ) guess )
                ( else ( helper ( + guess 1 ) n ))))

( define ( oddstart n )
         ( if ( even? n ) 
              ( + n 1 )
              n))

(define ( prime? n )
  ( if ( = ( smalldiv n ) n )
       #t
       #f ))
