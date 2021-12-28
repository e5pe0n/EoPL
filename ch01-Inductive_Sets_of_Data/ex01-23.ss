(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define list-index
  (lambda (pred lst)
    (list-index-n pred lst 0)
  )
)

(define list-index-n
  (lambda (pred lst n)
    (if (null? lst)
      #f
      (if (pred (car lst))
        n
        (list-index-n pred (cdr lst) (+ n 1))
      )
    )
  )
)

(print
  (list-index number? '(a 2 (1 3) b 7))
) ; 1
(print
  (list-index symbol? '(a (b c) 17 foo))
) ; 0
(print
  (list-index symbol? '(1 2 (a b) 3))
) ; #f
(print (list-index number? '())) ; #f
(print (list-index number? '(1)))  ; 0
(print (list-index number? '(a)))  ; #f