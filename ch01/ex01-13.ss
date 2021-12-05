(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define subst
  (lambda (new old slist)
    (if (null? slist)
      '()
      (map (mk-subst-in-s-exp new old) slist)
    )
  )
)
(define mk-subst-in-s-exp
  (lambda (new old)
    (lambda (sexp)
      (if (symbol? sexp)
        (if (eqv? sexp old)
          new
          sexp
        )
        (subst new old sexp)
      )
    )
  )
)

(print
  (subst 'a 'b '((b c) (b () d)))
) ; ((a c) (a () d))