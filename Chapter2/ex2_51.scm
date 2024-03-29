(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below (transform-painter
                         painter1
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 0.0)
                         split-point))
           (paint-up (transform-painter
                        painter2
                        split-point
                        (make-vect (1.0 0.5))
                        (make-vect (0.0 1.0)))))
      (lambda (frame)
        (paint-below frame)
        (paint-up frame)))))

(define (below1 painter1 painter2)
  (rotate-270
    (beside (rotate-90 painter2) (rotate-90 painter1))))
    
