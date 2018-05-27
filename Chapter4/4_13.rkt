#lang racket

;; Environment - 4.13
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment (list))

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (mcar frame))

(define (frame-values frame)
  (mcdr frame))


;; SPEC - Removes from any frame
;; Allows us to remove from the core implementation, i.e. remove set! from our language
;; No effect if the variable to remove is unfound
(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (mcar vars))
             (set-mcar! vars (mcdr vars))
             (set-mcar! vals (mcdr vals))
             'done]
            [else (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        #f
        (let [(frame (first-frame env))]
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; Tests
(define (mlist . args)
    (foldr mcons '() args))
(define e (mlist (mcons (mlist 'a 'b 'c) (mlist 'x 'y 'z))
                 (mcons (mlist 'd 'e 'f) (mlist 'x1 'y1 'z1))))
(eq? (make-unbound! 'a e) 'done)
(eq? (make-unbound! 'a e) #f)
(eq? (make-unbound! 'g e) #f)