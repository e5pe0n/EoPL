(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define count-occurences
  (lambda (s slist)
    (if (null? slist)
      0
      (+
        (count-x s (car slist))
        (count-occurences s (cdr slist))
      )
    )
  )
)
(define count-x
  (lambda (s x)
    (if (symbol? x)
      (if (eqv? x s)
        1
        0
      )
      (count-occurences s x)
    )
  )
)

(print
  (count-occurences 'x '((f x) y (((x z) x))))
) ; 3
(print
  (count-occurences 'x '((f x) y (((x z) () x))))
) ; 3
(print
  (count-occurences 'w '((f x ) y (((x z) x))))
) ; 0