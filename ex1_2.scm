( define ( square a ) ( * a a ))

( define ( sumsquares a b c ) ( + ( square a ) ( square b ) ( square c )))

( define ( minval a b c ) ( 
                        cond (( and ( <= a b ) ( <= a c )) a)
                        (( and ( <= b a ) ( <= b c )) b)
                        ( else c )))

( define ( sumsqtoptwo a b c ) ( - ( sumsquares a b c ) ( square ( minval a b c ))))
