(define-datatype s-list s-list?
  (an-s-list
    (sexps (list-of s-exp?))
  )
)
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
        (and (pair? val)
          (pred (car val))
          ((list-of pred) (cdr val))
        )
      )
    )
  )
)