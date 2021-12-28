(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define up
  (lambda (lst)
    (let f ([x '()] [lst lst])
      (if (list? x)
        (if (null? lst)
          x
          (let g [(x x)]
            (if (null? x)
              (f (car lst) (cdr lst))
              (cons (car x) (g (cdr x)))
            )
          )
        )
        (if (null? lst)
          (list x)
          (cons x (f (car lst) (cdr lst)))
        )
      )
    )
  )
)

(print
  (up '((1 2) (3 4)))
) ; (1 2 3 4)
(print
  (up '((1 2 3) (4 5 6) (7 8 9 10)))
) ; (1 2 3 4 5 6 7 8 9 10)
(print
  (up '(((1 2 3) (4 5 6)) (7 8 9 10)))
) ; ((1 2 3) (4 5 6) 7 8 9 10)
(print
  (up '((x (y)) z))
) ; (x (y) z)
(print
  (up '(((x (y))) z))
) ; ((x (y)) z)
(print
  (up '(a b c d))
) ; (a b c d)