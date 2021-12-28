(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define N 10)
(define zero '())
(define is-zero?
  (lambda (n)
    (null? n)
  )
)
(define successor
  (lambda (n)
    (if (is-zero? n)
      (cons 1 n)
      (let ([r (car n)] [q (cdr n)])
        (if (= r (- N 1))
          (cons 0 (successor q))
          (cons (+ r 1) q)
        )
      )
    )
  )
)
(define predecessor
  (lambda (n)
    (let ([r (car n)] [q (cdr n)])
      (if (= r 0)
        (cons (- N 1) (predecessor q))
        (if (and (= r 1) (is-zero? q))
          zero
          (cons (- r 1) q)
        )
      )
    )
  )
)

(define repr
  (lambda (n)
    (let f ([m n])
      (if (= m 0)
        zero
        (successor (f (- m 1)))
      )
    )
  )
)

; successor
; (print
;   (successor zero)
; )
; (print
;   (successor (successor zero))
; )
; (print
;   (successor (successor (successor zero)))
; )
; (print
;   (successor `(9 . ,zero))
; )
; (print
;   (successor (successor `(9 . ,zero)))
; )
; (print (successor (repr 100)))

; predecessor
; (print (predecessor (repr 3)))
; (print
;   (predecessor (predecessor (repr 3)))
; )
; (print
;   (predecessor (predecessor (predecessor (repr 3))))
; )
; (print
;   (predecessor (repr 10))
; )
; (print
;   (predecessor (repr 100))
; )

(define b-eqv?
  (lambda (b1 b2)
    (cond
      ((is-zero? b1) (is-zero? b2))
      ((is-zero? b2) #f)
      (else
        (let (
            [r1 (car b1)] [q1 (cdr b1)]
            [r2 (car b2)] [q2 (cdr b2)]
          )
          (if (= r1 r2)
            (b-eqv? q1 q2)
            #f
          )
        )
      )
    )
  )
)

(define b-less-than?
  (lambda (b1 b2)
    (cond
      ((is-zero? b2) #f)
      ((is-zero? b1) #t)
      (else (let (
          [r1 (car b1)] [q1 (cdr b1)]
          [r2 (car b2)] [q2 (cdr b2)]
        )
        (if (b-less-than? q1 q2)
          #t
          (if (b-eqv? q1 q2)
            (< r1 r2)
            #f
          )
        )
      ))
    )
  )
)

; (print
;   (b-less-than? (repr 0) (repr 0))
; )
; (print
;   (b-less-than? (repr 1) (repr 0))
; )
; (print
;   (b-less-than? (repr 0) (repr 1))
; )
; (print
;   (b-less-than? (repr 2) (repr 1))
; )
; (print
;   (b-less-than? (repr 1) (repr 2))
; )
; (print
;   (b-less-than? (repr 2) (repr 2))
; )
; (print
;   (b-less-than? (repr 11) (repr 10))
; )
; (print
;   (b-less-than? (repr 10) (repr 11))
; )
; (print
;   (b-less-than? (repr 913) (repr 391))
; )
; (print
;   (b-less-than? (repr 391) (repr 913))
; )
; (print
;   (b-less-than? (repr 913) (repr 991))
; )
; (print
;   (b-less-than? (repr 991) (repr 993))
; )
; (print
;   (b-less-than? (repr 993) (repr 991))
; )
; (print
;   (b-less-than? (repr 991) (repr 913))
; )

(define b-add
  (lambda (b1 b2)
    (let ([sm (if (b-less-than? b1 b2) b1 b2)] [lg (if (b-less-than? b1 b2) b2 b1)])
      (let f ([n sm])
        (if (is-zero? n)
          lg
          (successor (f (predecessor n)))
        )
      )
    )
  )
)
; (print
;   (b-add (repr 0) (repr 0))
; )
; (print
;   (b-add (repr 0) (repr 1))
; )
; (print
;   (b-add (repr 1) (repr 0))
; )
; (print
;   (b-add (repr 1) (repr 1))
; )
; (print
;   (b-add (repr 9) (repr 1))
; )
; (print
;   (b-add (repr 1) (repr 9))
; )
; (print
;   (b-add (repr 89) (repr 11))
; )
; (print
;   (b-add (repr 11) (repr 89))
; )
; (print
;   (b-add (repr 100) (repr 1))
; )
; (print
;   (b-add (repr 1) (repr 100))
; )

(define b-mul
  (lambda (b1 b2)
    (let ([sm (if (b-less-than? b1 b2) b1 b2)] [lg (if (b-less-than? b1 b2) b2 b1)])
      (let f ([n sm])
        (if (is-zero? n)
          zero
          (b-add lg (f (predecessor n)))
        )
      )
    )
  )
)
; (print
;   (b-mul (repr 0) (repr 0))
; )
; (print
;   (b-mul (repr 0) (repr 1))
; )
; (print
;   (b-mul (repr 1) (repr 0))
; )
; (print
;   (b-mul (repr 1) (repr 1))
; )
; (print
;   (b-mul (repr 2) (repr 3))
; )
; (print
;   (b-mul (repr 3) (repr 2))
; )
; (print
;   (b-mul (repr 3) (repr 4))
; )
; (print
;   (b-mul (repr 4) (repr 3))
; )
; (print
;   (b-mul (repr 73) (repr 37))
; )
; (print
;   (b-mul (repr 37) (repr 73))
; )

(define fact
  (lambda (n)
    (if (is-zero? n)
      (successor zero)
      (b-mul n (fact (predecessor n)))
    )
  )
)
(print (fact zero))
(print (fact (repr 1)))
(print (fact (repr 2)))
(print (fact (repr 3)))
(print (fact (repr 10)))  ; (0 0 8 8 2 6 3)