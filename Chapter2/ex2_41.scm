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

(define (unique-triples n)
  (flatmap (lambda (x)
             (map (lambda (y) (cons y x)) (enumerate-interval ( - (cadr x) 1)) ))
             (unique-pairs n)))

(define (sum? l n)
  ( = n (accumulate + 0 l)))

(define (triplets n s)
  (filter (lambda(x) (sum? x s)) (unique-triples n)))

(triplets 20 30)
