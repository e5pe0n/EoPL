(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define left
  (lambda (node)
    (cadr node)
  )
)
(define right
  (lambda (node)
    (caddr node)
  )
)

(define path
  (lambda (n bst)
    (let f ([root bst])
      (if (null? root)
        '()
        (let ([v (car root)])
          (cond
            ((= v n) '())
            ((< v n) (cons 'right (f (right root))))
            (else (cons 'left (f (left root))))
          )
        )
      )
    )
  )
)

(define bst1
  '(14
      (7
        ()
        (12 () ())
      )
      (26
        (20
          (17 () ())
          ()
        )
        (31 () ())
      )
    )
)

(print
  (path 17 bst1)
) ; (right left left)
(print
  (path 14 bst1)
) ; ()
(print
  (path 31 bst1)
) ; (right right)