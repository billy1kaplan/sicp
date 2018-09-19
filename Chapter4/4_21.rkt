#lang racket
; PART A

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (<= k 1)
          1
          (+ (fb fb (- k 1)) (fb fb (- k 2)))))))
 5)

; PART B
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

;; Y-operator
(define (f-y x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))