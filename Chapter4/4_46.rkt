#lang racket

; If amb were not searching from left-to-right,
; We would not be able to determine the order for our parsing.
; For example, look at the function (parse-sentence)
; (define (parse-sentence)
;   (list 'sentence
;         (parse-noun-phrase)
;         (parse-verb-phrase)))

; Since our order is to evaluate from left-to-right, we first parse for the noun phrase in the sentence
; and then parse for the verb phrase. The sentence that is being parsed is shared as a mutable global state,
; thus the order in which we attempt to parse for the noun-phrase and verb-phrase matter. If evaluated from right-to-left,
; we are performing our parsing backwards (looking for a verb-phrase and then a noun-phrase, which is not what we want).
