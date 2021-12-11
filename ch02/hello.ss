(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(print
  (let f ([n 10])
    (if (= n 0)
      1
      (* n (f (- n 1)))
    )
  )
) ; 3628800