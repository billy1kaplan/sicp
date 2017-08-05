( define ( make-interval a b ) ( cons a b ))

( define ( upperbound interval ) ( max ( car interval cdr interval)))

( define ( lowerbound interval ) ( min ( cdr interval )))
