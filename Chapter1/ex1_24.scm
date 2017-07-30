( define ( timed-prime-test3 n )
         (newline)
         (display n)
         (start-prime-test (oddstart n) 30 (runtime)))

(define (start-prime-test n count start-time)
  ( cond (( = count 0 ) (report-prime n ( - (runtime) start-time)))
         ((fast-prime?  n 10) (start-prime-test ( + n 2 ) ( - count 1 ) start-time ))
         ( else ( start-prime-test ( + n 2 ) count start-time ))))

(define (report-prime n elapsed-time)
  (newline)
  (display n )
  (newline)
  elapsed-time)

( define ( square n )
         ( * n n))

( define ( expmod base exp m )
         ( cond (( = exp 0 ) 1)
                (( even? exp )
                 ( remainder
                   ( square (expmod base ( / exp 2 ) m ))
                   m))
                 ( else
                   ( remainder
                     ( * base ( expmod base ( - exp 1 ) m ))
                     m))))

( define ( fermet-test n )
         ( define ( try-it a )
                  ( = ( expmod a n n ) a ))
         ( try-it ( + 1 ( random ( - n 1 )))))

( define ( fast-prime? n times )
         ( cond (( = times 0 ) true )
                (( fermet-test n )
                 ( fast-prime? n ( - times 1 )))
                ( else #f )))

( define ( oddstart n )
         ( if ( even? n ) ( + 1 n )
              n))

