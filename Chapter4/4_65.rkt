#lang racket
;; Wheel Rule:
;; (rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;;            (supervisor ?x ?middle-manager)))


;; Oliver Warbucks is listed 4 times due to the following relationships:
;; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;;      (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

;; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;;      (supervisor (Fect Cy D) (Bitdiddle Ben)))

;; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;;      (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

;; (and (supervisor (Scrooge Eben) (Warbucks Oliver))
;;      (supervisor (Cratchet Robert) (Scrooge Eben)))

;; Thus, there are 4 distinct paths that classify Oliver as a wheel.
