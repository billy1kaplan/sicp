(define (square n) ( * n n ))

(define (cube n) ( * n n n))

(define (abs n)
  ( if ( < n 0 )
       (- n)
       n))

(define (goodenough guess x)
  (if ( < ( abs ( - (cube guess) x)) 0.001 )
    #t
    #f))

(define (improvedguess y x)
   ( / ( + ( / x ( square y ) ) ( * 2 y)) 3))

(define (cube-iter guess x)
  (if (goodenough guess x)
    guess
    (cube-iter (improvedguess guess x) x)))

(define (cubert x)
  (cube-iter 1.0 x))
