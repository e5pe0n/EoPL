(require-for-syntax 'datatype)

(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)
  )
)
(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?)
  )
  (s-list-s-exp
    (slst s-list?)
  )
)