(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define filter-in
  (lambda (pred lst)
    (if (null? lst)
      '()
      (if (pred (car lst))
        (cons
          (car lst)
          (filter-in pred (cdr lst))
        )
        (filter-in pred (cdr lst))
      )
    )
  )
)

(print
  (filter-in number? '(a 2 (1 3) b 7))
) ; (2 7)
(print
  (filter-in symbol? '(a (b c) 17 foo))
) ; (a foo)
(print (filter-in number? '())) ; ()
(print (filter-in number? '(1)))  ; (1)
(print (filter-in number? '(a)))  ; ()