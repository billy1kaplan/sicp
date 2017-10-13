(define (enumerate-interval start end)
  ( if (> start end)
       '()
       (cons start (enumerate-interval ( + start 1 ) end))))

(define (accumulate init op collection)
  (if (null? collection)
    init
    (op (car collection)
        (accumulate init op (cdr collection)))))

(define (flatmap func collection)
  (accumulate '() append (map func collection)))

(define empty-board '())

(define (safe? k positions)
  (newline)
  (display positions)
  ( if ( = k 1)
#t
  (let ((row (car positions)))
  (define (iter col remaining_pos)
    (cond ((null? remaining_pos) #t)
          ((or (= row (car remaining_pos)) (= (car remaining_pos) ( - row col )) (= (car remaining_pos) ( + row col ))) #f)
          (else (iter ( + col 1) (cdr remaining_pos))))
    )
  (iter 1 (cdr positions)))))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions)
          (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row
                     k
                     rest-of-queens))
                 (enumerate-interval
                   1
                   board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
