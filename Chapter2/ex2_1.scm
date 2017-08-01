( define ( gcd n d )
         ( if ( = d 0 )
              n
              ( gcd d ( remainder n d ))))

( define ( make-rat n d )
         ( let (
                ; Consider case of GCD that returns a negative value
                ( g ( abs ( gcd n d )))
                ( sign 
                  ( if ( not ( equal? ( < 0 n ) ( < 0 d )))
                       ( - 1 )
                       1 ))
                )
               ( cons 
                 ( * sign ( / n g ))
                 ( abs ( / d g ))
                 )
               )
         )

( define ( numer x )
         ( car x ))

( define ( denom x )
         ( cdr x ))

( define ( print-rat x )
         ( newline )
         ( display ( numer x ))
         ( display "/" )
         ( display ( denom x ))
         ( newline )
         #t)

( define x (make-rat 1 2 ))
( print-rat x )
