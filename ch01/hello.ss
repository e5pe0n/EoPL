(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(print
  (append '(1 2 3) (list 10) '(4 5 6))
)
