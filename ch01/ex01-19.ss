(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define list-set
  (lambda (lst n x)
    (if (null? lst)
      '()
      (cons
        (if (eqv? n 0)
          x
          (car lst)
        )
        (list-set (cdr lst) (- n 1) x)
      )
    )
  )
)

(print
  (list-set '(a b c d) 2 '(1 2))
) ; (a b (1 2) d)
(print
  (list-ref
    (list-set '(a b c d) 3 '(1 5 10))
    3
  )
) ; (1 5 10)