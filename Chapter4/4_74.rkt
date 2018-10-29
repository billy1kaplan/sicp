#lang racket

;; Defining syntax rules for delayed stream evaluation
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;; Scheme Stream API
(define (force exp) (exp))

(define the-empty-stream '())

(define (stream-null? s)
  (null? s))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                            (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (void)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each (lambda (el) (display el) (newline)) s))


(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

;; Part a
;; Remove empty streams
;; Pull element out of singleton-streams
(define (simple-flatten stream)
  (stream-map stream-car
	      (stream-filter (lambda (x) (not (stream-null? x))) stream)))

;; Does the query system behave differently?
;; Since we are dealing with streams of 0 and 1 elements, the behavior will be identical, consider:
;; ((x1) () () (x2) (x3))

;; Regardless of using simple-flatten or flatten stream, we still stream the final elements as (x1 x2 x3)
