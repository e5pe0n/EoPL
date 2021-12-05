(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define list-sum
  (lambda (loi)
    (if (null? loi)
      0
      (+
        (car loi)
        (list-sum (cdr loi))
      )
    )
  )
)

(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
      (vector-ref v 0)
      (+
        (vector-ref v n)
        (partial-vector-sum v (- n 1))
      )
    )
  )
)
(define vector-sum
  (lambda (v)
    (let ([n (vector-length v)])
      (if (zero? n)
        0
        (partial-vector-sum v (- n 1))
      )
    )
  )
)

(print
  (list-sum '(1 2 3 4 5))
) ; 15
(print
  (vector-sum #(1 2 3 4 5))
) ; 15