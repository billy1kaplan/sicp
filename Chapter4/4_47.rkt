#lang racket

; This does not work, we will keep nesting (amb (parse-word verbs) (list 'verb-phrase (parse-verb-phrase) (parse-prepositional-phrase)))
; With each nesting, we will hit (parse-word verbs) which is different behavior than the original function.
; This causes an infinite loop. Interestingly, we will be able to find some matches; however, we will never run out of options to keep searching.

; For this particular case, the ordering does matter since it will determine the pattern we try to match. 
; We will hit an infinite loop due to the infinite nesting of amb. (our program will always have another option to try forever and we will never try
; the case of (parse-word verbs)
