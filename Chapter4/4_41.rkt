#lang racket

(define (baker f) (car f))
(define (cooper f) (cadr f))
(define (fletcher f) (caddr f))
(define (miller f) (cadddr f))
(define (smith f) (cadr (cdddr f)))

(define floors (list 1 2 3 4 5))

(define (flatmap f list)
  (apply append (map f list)))

(define (permutations n items)
  (define (generate-internal n acc)
    (cond ((= n 0) '())
          ((= n 1) acc)
          (else (generate-internal (- n 1)
                                   (flatmap (lambda (acc-val)
                                              (map (lambda (val) (cons val acc-val))
                                                   (filter (lambda (val) (not (member val acc-val))) items)))
                                            acc)))))
  (generate-internal n (map list items)))

(define requirements (lambda (floor)
                       (and (not (= (baker floor) 5))
                            (not (= (cooper floor) 1))
                            (not (= (fletcher floor) 5))
                            (not (= (fletcher floor) 1))
                            (> (miller floor) (cooper floor))
                            (not (= (abs (- (smith floor) (fletcher floor))) 1))
                            (not (= (abs (- (fletcher floor) (cooper floor))) 1)))))

(filter requirements (permutations 5 floors))
