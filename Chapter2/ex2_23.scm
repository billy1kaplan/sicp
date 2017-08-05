( define ( for-each func lst )
         ( func ( car lst ))
         ( if ( null? lst )
              #t
              ( for-each ( cdr lst ))))

( define ( for-each func lst )
         ( if ( null? lst )
              #t
              ( let ()
                    ( func ( car lst ))
                    ( for-each func ( cdr lst )))))

(for-each 
  (lambda (x) (newline) (display x))
  (list 57 321 88))
