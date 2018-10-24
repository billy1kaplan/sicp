#lang racket

;; Yes, he is correct that we will reduce the number of explored possibilities
;; Consider the range 1-10. Any sum of two values > 100 will instantly be discarded, so we ignore the
;; space i**2 + j**2 > 10**2
;; This is the formulat for a circle, so we thus ignore any solutions outside
;; where R = range of numbers considerd
;; We can calculate that we consider pi*(R/2)**2 / (R * R) -> pi/4 of the solution space is considered, so we discard (4-pi)/4 = ~21.4%
;; This space would be searched by our previous implementation