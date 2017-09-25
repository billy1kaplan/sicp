( define ( make-mobile left right )
         ( list left right ))

( define ( make-branch length structure )
         ( list length structure ))

( define ( left-branch mobile )
         ( car mobile ))

( define ( right-branch mobile )
         ( car ( cdr mobile )))

( define ( branch-length branch )
         ( car branch ))

( define ( branch-structure branch )
         ( car ( cdr branch )))

( define ( total-weight mobile )
         ( cond (( null? mobile ) 0 )
                (( not ( pair? ( right-branch mobile ))) ( right-branch mobile ))
                ( else ( + ( total-weight ( left-branch mobile )) ( total-weight ( right-branch mobile ))))))

( define ( balanced? mobile )
         ( define ( helper mobile )
                  ( cond (( null? mobile ) 0 )
                         (( not ( pair? ( right-branch mobile ))) ( * ( branch-length mobile ) ( right-branch mobile )))
                         ( else ( + ( helper ( left-branch mobile )) ( helper ( right-branch mobile ))))))
         ( cond (( null? mobile ) #t )
                (( not ( pair? ( right-branch mobile ))) #t )
                ( else ( = ( helper ( left-branch mobile )) ( helper ( right-branch mobile ))))))

( define b1 ( make-branch 5 5 ))
( define b2 ( make-branch 7 8 ))
( define b3 ( make-branch b1 b2 ))
( define b4 ( make-branch b3 b2))

( define x ( make-mobile b4 b1))

;( right-branch x )
;( total-weight x )
( balanced? x )

;4
; you would need to just change the selectors 
