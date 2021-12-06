(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define exists?
  (lambda (pred lst)
    (if (null? lst)
      #f
      (if (pred (car lst))
        #t
        (exists? pred (cdr lst))
      )
    )
  )
)

(print
  (exists? number? '(a b c 3 e))
) ; #t
(print
  (exists? number? '(a b c d e))
) ; #f
(print
  (exists? number? '(a))
) ; #f
(print
  (exists? number? '(1))
) ; #t
(print
  (exists? number? '())
) ; #f