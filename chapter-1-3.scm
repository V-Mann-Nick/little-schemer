(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
  )
)

(print "==== Testing some stuff ====")
(print (atom? (quote ())))
(print (atom? "hello"))
(print (car (quote (hello world))))
(print (cdr (quote (hello world))))
(print (null? (quote ())))
(print "")

(print "==== List of atoms - lat? ====")
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
(print (lat? (quote (hello world (again)))))
(print "")

(print "==== Member? ====")
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
(print "")

(print "==== Remove member - rember ====")
(define rember
  (lambda (i l)
    (cond
      ((null? l) '())
      ((eq? i (car l)) (cdr l))
      (else 
        (cons
          (car l)
          (rember i (cdr l)))
        )
    )
  )
)

(print (rember 'c '(a b c d)))
(print "")


(print "==== Firsts ====")
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
        (cons
          (car (car l))
          (firsts (cdr l))
        )
      )
    )
  )
)

(print (firsts '((a b) (c d) (e f))))
(print "")

(print "==== insertR - insert right ====")
(define insertR
  (lambda (item after l)
    (cond
      ((null? l) '())
      (
        (eq? after (car l))
        (cons after (cons item (cdr l)))
      )
      (else 
        (cons
          (car l)
          (insertR item after (cdr l))
        )
      )
    )
  )
)

(print (insertR 'e 'd '(a b c d f)))
(print "")


(print "==== insertL - insert left ====")
(define insertL
  (lambda (item before l)
    (cond
      ((null? l) '())
      (
        (eq? before (car l))
        (cons item l)
      )
      (else
        (cons
          (car l)
          (insertL item before (cdr l))
        )
      )
    )
  )
)

(print (insertL 'd 'e '(a b c e f)))
(print "")

(print "==== subst - substitute ====")
; Compsing previous functions
; (define subst
;   (lambda (new old l)
;     (rember
;       old
;       (insertL new old l)
;     )
;   )
; )

; Recursively
(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons new (cdr l)))
      (else
        (cons
          (car l)
          (subst new old (cdr l))
        )
      )
    )
  )
)

(print (subst 'new 'old '(a old b)))
(print "")

(print "==== subst2 ====")
(define subst2
  (lambda (new o1 o2 l)
    (cond
      ((null? l) '())
      (
        (or 
          (eq? o1 (car l))
          (eq? o2 (car l))
        )
        (cons new (cdr l))
      )
      (else
        (cons
          (car l)
          (subst2 new o1 o2 (cdr l))
        )
      )
    )
  )
)

(print (subst2 'new 'o1 'o2 '(a b o1 c o2)))
(print (subst2 'new 'o1 'o2 '(a b o2 c o1)))
(print "")

(print "==== multirember - remove all occurences of member ====")
(define multirember
  (lambda (i l)
    (cond
      ((null? l) '())
      ((eq? i (car l)) (multirember i (cdr l)))
      (else 
        (cons
          (car l)
          (multirember i (cdr l)))
        )
    )
  )
)

(print (multirember 'c '(a b c d c e c)))
(print "")

(print "==== multiinsertR ====")
(define multiinsertR
  (lambda (item after l)
    (cond
      ((null? l) '())
      (
        (eq? after (car l))
        (cons after (cons item (multiinsertR item after (cdr l))))
      )
      (else 
        (cons
          (car l)
          (multiinsertR item after (cdr l))
        )
      )
    )
  )
)

(print (multiinsertR 'e 'd '(a b c d f g d)))
(print "")

(print "==== multiinsertL ====")
(define multiinsertL
  (lambda (item before l)
    (cond
      ((null? l) '())
      (
        (eq? before (car l))
        (cons item (cons before (multiinsertL item before (cdr l))))
      )
      (else 
        (cons
          (car l)
          (multiinsertL item before (cdr l))
        )
      )
    )
  )
)

(print (multiinsertL 'd 'e '(a b c e f g e)))
(print "")

(print "==== multisubst - substitute all occurences ====")
(define multisubst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons new (multisubst new old (cdr l))))
      (else
        (cons
          (car l)
          (multisubst new old (cdr l))
        )
      )
    )
  )
)

(print (multisubst 'new 'old '(a old b old)))
(print "")
