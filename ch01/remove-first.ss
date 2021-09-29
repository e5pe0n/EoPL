(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define remove-first
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? s (car los))
        (cdr los)
        (cons (car los) (remove-first s (cdr los)))
      )
    )
  )
)