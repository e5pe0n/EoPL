(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define list-length
  (lambda (lst)
    (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))
    )
  )
)