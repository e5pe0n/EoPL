(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define sort/predicate
  (lambda (pred loi)
    (let sort ([loi loi])
      (if (null? loi)
        '()
        (let ([pivot (car loi)])
          (let f ([former '()] [latter '()] [lst (cdr loi)])
            (if (null? lst)
              (append (sort former) (list pivot) (sort latter))
              (let ([x (car lst)])
                (if (pred x pivot)
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
)

(print
  (sort/predicate < '(8 2 5 2 3))
) ; (2 2 3 5 8)
(print
  (sort/predicate > '(8 2 5 2 3))
) ; (8 5 3 2 2)
(print
  (sort/predicate < '(8 2))
) ; (2 8)
(print
  (sort/predicate > '(8 2))
) ; (8 2)
(print
  (sort/predicate < '(8))
) ; (8)
(print
  (sort/predicate > '(8))
) ; (8)
(print
  (sort/predicate < '(8 8 8 8 8))
) ; (8 8 8 8 8)
(print
  (sort/predicate > '(8 8 8 8 8))
) ; (8 8 8 8 8)