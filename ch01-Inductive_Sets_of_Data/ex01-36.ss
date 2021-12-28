(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define g
  (lambda (x lst)
    (cons
      x
      (let f ([lst lst])
        (if (null? lst)
          '()
          (cons
            (let ([y (car lst)])
              (list (+ (car y) 1) (cadr y))
            )
            (f (cdr lst))
          )
        )
      )
    )
  )
)

(define number-elements
  (lambda (lst)
    (if (null? lst)
      '()
      (g
        (list 0 (car lst))
        (number-elements (cdr lst))
      )
    )
  )
)


(print
  (number-elements '(a b c d e))
); ((0 a) (1 b) (2 c) (3 d) (4 e))