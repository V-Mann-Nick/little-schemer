(print "==== add1 ====")
(define add1
  (lambda (n)
    (+ n 1)
  )
)

(print (add1 5))
(print "")


(print "==== sub1 ====")
(define sub1
  (lambda (n)
    (- n 1)
  )
)

(print (sub1 4))
(print (zero? 0))
(print "")

(print "==== + ====")
(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else 
        (add1 (o+ a (sub1 b)))
      )
    )
  )
)

(print (o+ 5 2))
(print "")

(print "==== - ====")
(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
        (sub1 (o- a (sub1 b)))
      )
    )
  )
)

(print (o- 5 2))
(print "")

(print "==== addtup ====")
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
        (o+ (car tup) (addtup (cdr tup)))
      )
    )
  )
)

(print (addtup '(2 4 3 10)))
(print "")

(print "==== ox ====")
(define ox
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else
        (o+ a (ox a (sub1 b)))
      )
    )
  )
)

(print (ox 3 4))
(print "")

(print "==== tup+ ====")
(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else
        (cons 
          (o+ (car t1) (car t2))
          (tup+ (cdr t1) (cdr t2))
        )
      )
    )
  )
)

(print (tup+ '(5 1 3) '(1 3 3)))
(print (tup+ '(5 1) '(3 3 4)))
(print "")

(print "==== > ====")
(define o>
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (o> (sub1 a) (sub1 b)))
    )
  )
)

(print (o> 5 4))
(print "")

(print "==== < ====")
(define o<
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (o< (sub1 a) (sub1 b)))
    )
  )
)

(print (o< 3 4))
(print "")

(print "==== = ====")
(define o=
  (lambda (a b) 
    (and 
      (not (o< a b))
      (not (o> a b))
    )
  )
)

(print (o= 3 2))
(print "")

(print "==== pow ====")
(define pow
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else 
        (ox a (pow a (sub1 b)))
      )
    )
  )
)

(print (pow 5 3))
(print "")

(print "==== / ====")
(define o/
  (lambda (a b)
    (cond
      ((o< a b) 0)
      (else
        (add1 (o/ (o- a b) b))
      )
    )
  )
)

(print (o/ 15 5))
(print "")

(print "==== len ====")
(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (len (cdr l))))
    )
  )
)

(print (len '(5 1 2 3 4)))
(print "")

(print "==== pick ====")
(define pick
  (lambda (n l)
    (cond
      ((zero? (sub1 n)) (car l))
      (else
        (pick (sub1 n) (cdr l))
      )
    )
  )
)

(print (pick 1 '(1 2 3)))

(print "==== rempick ====")
(define rempick
  (lambda (n l)
    (cond
      ((null? l) '())
      ((zero? (sub1 n)) (cdr l))
      (else
        (cons
          (car l)
          (rempick (sub1 n) (cdr l))
        )
      )
    )
  )
)

(print (rempick 5 '(1 2 3 4)))


(print "==== no-nums ====")
(define no-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (no-nums (cdr l)))
      (else
        (cons
          (car l)
          (no-nums (cdr l))
        )
      )
    )
  )
)

(print (no-nums '(apple 1 pear 2 banana)))
(print "")

(print "==== all-nums ====")
(define all-nums
  (lambda (l)
    (cond
      ((null? l) '())
      (
        (number? (car l))
        (cons 
          (car l) 
          (all-nums (cdr l))
        )
      )
      (else (all-nums (cdr l)))
    )
  )
)

(print (all-nums '(apple 1 pear 2 banana)))
(print "")

(print "==== eqan? ====")
(define eqan?
  (lambda (a b)
    (cond
      (
        (and (number? a) (number? b))
        (o= a b)
      )
      (
        (or (number? a) (number? b))
        #f
      )
      (else (eq? a b))
    )
  )
)

(print (eqan? 'a 'a))
(print (eqan? 'a 'b))
(print (eqan? '1 'b))
(print (eqan? '1 '2))
(print (eqan? '1 '1))

(print "==== occur ====")
(define occur
  (lambda (a l)
    (cond
      ((null? l) 0)
      (
        (eqan? a (car l))
        (add1 (occur a (cdr l)))
      )
      (else (occur a (cdr l)))
    )
  )
)

(print (occur 'a '(1 1 a 1 a)))
(print (occur 1 '(1 1 a 1 a)))
(print "")

(print "==== one? ====")
(define one?
  (lambda (n)
    (cond
      ((zero? (sub1 n)) #t)
      (else #f)
    )
  )
)

(print (one? 1))
(print (one? 5))
