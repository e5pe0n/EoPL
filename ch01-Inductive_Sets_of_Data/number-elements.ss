(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
      '()
      (cons
        (list n (car lst))
        (number-elements-from (cdr lst) (+ n 1))
      )
    )
  )
)
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)
  )
)

(print
  (number-elements '(a b c d e))
) ; ((0 a) (1 b) (2 c) (3 d) (4 e))