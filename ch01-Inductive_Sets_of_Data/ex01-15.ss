(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define duple
  (lambda (n x)
    (if (eqv? n 0)
      '()
      (cons
        x
        (duple (- n 1) x)
      )
    )
  )
)

(print
  (duple 2 3)
) ; (3 3)
(print
  (duple 4 '(ha ha))
) ; ((ha ha) (ha ha) (ha ha) (ha ha))
(print
  (duple 0 '(blah))
) ; ()