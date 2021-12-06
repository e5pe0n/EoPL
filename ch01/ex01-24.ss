(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define every?
  (lambda (pred lst)
    (if (null? lst)
      #t
      (if (pred (car lst))
        (every? pred (cdr lst))
        #f
      )
    )
  )
)

(print
  (every? number? '(a b c 3 e))
) ; #f
(print
  (every? number? '(1 2 3 4 5))
) ; #t
(print
  (every? number? '(1))
) ; #t
(print
  (every? number? '(a))
) ; #f
(print
  (every? number? '())
) ; #t