(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)


(define sort
  (lambda (loi)
    (if (null? loi)
      '()
      (let ([pivot (car loi)])
        (let f ([former '()] [latter '()] [lst (cdr loi)])
          (if (null? lst)
            (append (sort former) (list pivot) (sort latter))
            (let ([x (car lst)])
              (if (< x pivot)
                (f (cons x former) latter (cdr lst))
                (f former (cons x latter) (cdr lst))
              )
            )
          )
        )
      )
    )
  )
)

(print
  (sort '(8 2 5 2 3))
) ; (2 2 3 5 8)
(print
  (sort '(8 2 5 2 3 1))
) ; (1 2 2 3 5 8)
(print
  (sort '(8 8 8 8 8))
) ; (8 8 8 8 8)
(print
  (sort '(8 2))
) ; (2 8)
(print
  (sort '(8))
) ; (8)