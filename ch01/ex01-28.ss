(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      (else
        (let ([x (car loi1)] [y (car loi2)])
          (if (<= x y)
            (cons
              x
              (merge (cdr loi1) loi2)
            )
            (cons
              y
              (merge loi1 (cdr loi2))
            )
          )
        )
      )
    )
  )
)

(print
  (merge '(1 4) '(1 2 8))
) ; (1 1 2 4 8)
(print
  (merge '(35 62 81 90 91) '(3 83 85 90))
) ; (3 35 62 81 83 85 90 90 91)
(print
  (merge '(1 4) '())
) ; (1 4)
(print
  (merge '() '(1 4))
) ; (1 4)
(print
  (merge '() '())
); ()