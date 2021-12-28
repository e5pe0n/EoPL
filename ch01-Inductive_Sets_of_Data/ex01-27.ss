(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define flatten
  (lambda (slist)
    (if (null? slist)
      '()
      (let ([x (car slist)])
        (if (list? x)
          (if (null? x)
            (flatten (cdr slist))
            (let f ([lst (flatten x)])
              (if (null? lst)
                (flatten (cdr slist))
                (cons
                  (car lst)
                  (f (cdr lst))
                )
              )
            )
          )
          (cons
            x
            (flatten (cdr slist))
          )
        )
      )
    )
  )
)

(print
  (flatten '(a b c))
) ; (a b c)
(print
  (flatten '((a) () (b ()) () (c)))
) ; (a b c)
(print
  (flatten '((a b) c (((d)) e)))
) ; (a b c d e)
(print
  (flatten '(a b (() (c))))
) ; (a b c)
(print
  (flatten '((a)))
) ; (a)
(print
  (flatten '(a))
) ; (a)
(print
  (flatten '())
) ; ()