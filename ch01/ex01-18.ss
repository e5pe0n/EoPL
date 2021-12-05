(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
      '()
      (cons
        (let ([x (car slist)])
          (if (symbol? x)
            (cond
              ((eqv? x s1) s2)
              ((eqv? x s2) s1)
              (else x)
            )
            (swapper s1 s2 x)
          )
        )
        (swapper s1 s2 (cdr slist))
      )
    )
  )
)

(print
  (swapper 'a 'd '(a b c d))
) ; (d b c a)
(print
  (swapper 'a 'd '(a d () c d))
) ; (d a () c a)
(print
  (swapper 'x 'y '((x) y (z (x))))
) ; ((y) x (z (y)))