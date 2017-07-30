(define (square n) ( * n n ))

(define (goodenough guess x)
  (if ( < ( square ( - (square guess) x)) 0.0001 )
    #t
    #f))

(define (goodenoughplus guess prevguess x)
  (if ( < ( / ( square ( - guess prevguess )) x) 0.0001 )
  #t
  #f))

(define (average a b) ( / ( + a b ) 2 ))

(define (improvedguess guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (goodenough guess x)
    guess
    (sqrt-iter (improvedguess guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iterplus guess x)
  ( if (goodenoughplus (improvedguess guess x) guess x)
       guess
       (sqrt-iterplus (improvedguess guess x) x)))

( define (sqrt1 x)
         (sqrt-iterplus 1.0 x))
