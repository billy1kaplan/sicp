(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    ( op ( car sequence )
         (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map2 * v w)))

(define (map2 op v w)
  (if (null? v)
    '()
    (cons (op (car v) (car w)) (map2 op (cdr v) (cdr w)))))

(let ((x  (list 7 11))
      (y (list 13 17)))
  (dot-product x y))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(let ((x  (list 2 3))
      (y (list (list 13 17) (list 9 5))))
  (matrix-*-vector y x))

(define (transpose mat)
  (accumulate-n cons '() mat))

(let ((x  (list 2 3))
      (y (list (list 13 17) (list 9 5))))
  (transpose y))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(let ((x  (list (list 2 3) (list 7 11)))
      (y (list (list 13 17) (list 9 5))))
  (matrix-*-matrix x y))
