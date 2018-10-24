#lang racket

;; The order of restrictions does not affect the answer. We need to simultaneously satisfy all of the properties. Thus,
;; our requirements are associate (And is a Monoid?)

;; Reordering the restrictions will not significantly speed up our program. Although, we may find an optimal way to fail some of our restrictions faster,
;; this is not likely to result in a significant speed up. We have 8 requirements. Through optimal ordering, we could eliminate some unnecessary work here,
;; but I do not think that these comparisons are the bottleneck.

;; BUT, one requirement does take a signicant more amount of work: checking distinct?. Since this runs in O(n^2) time, moving this to the end will give
;; a significant speed boost.