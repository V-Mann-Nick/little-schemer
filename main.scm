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

; (print (lat? (quote ())))
; (print (lat? (quote (hello world again))))
; (print (lat? (quote (hello world (again)))))

; My implementation
(define member?
  (lambda (i l)
    (cond
      ((null? l) #f)
      ((eq? i (car l)) #t)
      (else (member? i (cdr l)))
    )
  )
)

; Book implementation
; (define member?
;   (lambda (a lat)
;     (cond
;       ((null? lat) #f)
;       (else 
;         (or
;           (eq? (car lat) a)
;           (member? a (cdr lat))
;         )
;       )
;     )
;   )
; )

(print (member? 'hello '(hello)))
(print (member? 'hello '(hell world)))
