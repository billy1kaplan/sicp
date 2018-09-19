#lang racket
;; Alternative form
; (let ((u '*unassigned*)
;       (v '*unassigned*))
;   (let ((a <e1>)
;         (b <e2>))
;     (set! u a)
;     (set! v b))
;   (<e3>))

; pre-expansion:
; (define (solve f y0 dt)
;   (define y (integral (delay dy) y0 dt))
;   (define dy (stream-map f y))
;   y)

; post-expansion:
; (define (solve f y0 dt)
;   (let ((y '*unassigned*)
;         (dy '*unassigned*))
;     (let ((a (integral (delay dy) y0 dt))
;           (b (stream-map f y))
;       (set! u a)
;       (set! v b))
;     y)

; post-expansion:
; (define (solve f y0 dt)
;   (let ((y '*unassigned*)
;         (dy '*unassigned*))
;       (set! y (integral (delay dy) y0 dt)
;       (set! dy (stream-map f y))
;       y)

;; This alternative expansion will not work. The expression (b (stream-map f y)) will throw an error because y is still set to undefined. In the original form,
;; y has already been set to the proper value.