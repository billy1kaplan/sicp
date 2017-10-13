(define (accumulate op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (flatmap f s)
  (accumulate append '() (map f s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
               (map (lambda (p)
                      (cons x p))
                    (permutations
                      (remove x s))))
             s)))

(define (enumerate-interval n)
    (if (< n 1)
      '()
      (cons n (enumerate-interval ( - n 1 )))))

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y)) (enumerate-interval ( - x 1 ))))
           (enumerate-interval n)))

(display row)
(newline)
(define (prime-sum? pair)
  (prime? ( + (car pair) (cadr pair))))

(define (prime? n)
  (define (iter i)
    (cond (( > ( * i i) n)#t)
          (( = (remainder n i) 0) #f)
          (else (iter ( + i 1)))))
  (iter 2))


(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        ( + (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 12)
