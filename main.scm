(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
  )
)

; (print (atom? (quote ())))
; (print (atom? "hello"))
; (print (car (quote (hello world))))
; (print (cdr (quote (hello world))))
; (print (null? (quote ())))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
    )
  )
)

(print (lat? (quote ())))
(print (lat? (quote (hello world again))))
(print (lat? (quote (hello world (quote (again))))))
