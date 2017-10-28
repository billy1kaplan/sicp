#lang sicp

(car ''abracadabra)

(car (quote (quote abracadabra)))

;This is represented as (car (quote abracadabra))
;Which is quote i.e. (car (quote (a b)) -> a
(car (quote (a b)))
(car '(a b))
