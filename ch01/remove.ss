(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define remove
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? s (car los))
        (remove s (cdr los))
        (cons (car los) (remove s (cdr los)))
      )
    )
  )
)

(print
  (remove 'd '(a b c d e f d d a b c d))
) ; (a b c e f a b c)
(print
  (remove 'd '())
) ; ()
(print
  (remove 'd '(d d d d d))
) ; ()