#lang racket
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)

; This will never terminate ->
;(unless (= n 1)
;    (* n (factorial (- n 1)))
;    1))
; This expression will strictly evaluate the (* n (factorial (- n 1)))
; continuing on indefinitely