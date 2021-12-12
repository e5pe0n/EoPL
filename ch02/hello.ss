(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(print
  (if 1
    'a
    'b
  )
)