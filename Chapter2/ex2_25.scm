
(define x ( list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))


;wtf
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
