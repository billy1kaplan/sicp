( define ( sum term a next b )
         ( if ( > a b )
              0
              ( + ( term a )
                  ( sum term ( next a ) next b ))))

( define ( cube n )
         ( * n n n ))

( define ( integral f a b n )
         ( define h
                  ( / ( - ( * 1.0 b ) a ) n))
         ( define ( add-h k )
                  ( + ( * 2 h ) k ))
         ( * ( + ( f a ) ( f b ) )
             ( + ( * 2 ( sum cube ( + a ( * 2 h )) add-h ( - b h  ))) 
                 ( * 4 ( sum cube ( + a h ) add-h ( - b h ))))
             ( / h 3 )))

( define ( inc k )
         ( + 1 k ))

( define ( integrate f a b n )
         ( define h
                  ( / ( - ( * 1.0 b ) a ) n))
         ( define ( simpson k )
                  ( define y ( f ( + a ( * k h ))))
                  ( if ( or ( = 0 k ) ( = n k ))
                       y
                       ( if (even? k )
                            ( * 2 y )
                            ( * 4 y ))
                       ))
         ( * ( / h 3 ) ( sum simpson 0 inc n )))


 (define (simpson f a b n) 
   (define h (/ (- b a) n)) 
   (define (simpson-term k) 
     (define y (f (+ a (* k h)))) 
     (if (or (= k 0) (= k n)) 
         (* 1 y) 
         (if (even? k) 
             (* 2 y) 
             (* 4 y)))) 
   (* (/ h 3) (sum simpson-term 0 inc n)))


;; Testing 
(simpson cube 0 1 100) 
(integrate cube 0 1 1000) 
