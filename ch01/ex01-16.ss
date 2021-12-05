(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define invert
  (lambda (lst)
    (map swap lst)
  )
)
(define swap
  (lambda (lst)
    (list (cadr lst) (car lst))
  )
)

(print
  (invert '((a 1) (a 2) (1 b) (2 b)))
) ; ((1 a) (2 a) (b 1) (b 2))
(print
  (invert '((a 1) (2 b)))
) ; ((1 a) (b 2))
