(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define down
  (lambda (lst)
    (if (null? lst)
      '()
      (cons
        (down-x (car lst))
        (down (cdr lst))
      )
    )
  )
)
(define down-x
  (lambda (x)
    (list x)
  )
)

(print
  (down '(1 2 3))
) ; ((1) (2) (3))
(print
  (down '((a) (fine) (idea)))
) ; (((a)) ((fine)) ((idea)))
(print
  (down '(a (more (complicated)) object))
) ; ((a) ((more (complicated))) (object))