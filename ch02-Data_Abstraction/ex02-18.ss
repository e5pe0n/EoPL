(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define number->bintree
  (lambda (n)
    (list n '() '())
  )
)
(define current-element
  (lambda (node)
    (car node)
  )
)
(define move-to-left-son
  (lambda (node)
    (cadr node)
  )
)
(define move-to-right-son
  (lambda (node)
    (caddr node)
  )
)
(define at-leaf?
  (lambda (node)
    (null? node)
  )
)
(define insert-to-left
  (lambda (n node)
    (list
      (current-element node)
      (list n (move-to-left-son node) '())
      (move-to-right-son node)
    )
  )
)
(define insert-to-right
  (lambda (n node)
    (list
      (current-element node)
      (move-to-left-son node)
      (list n (move-to-right-son node) '())
    )
  )
)

(define t1
  (insert-to-right 14
    (insert-to-left 12
      (number->bintree 13)
    )
  )
)
(print t1)  ; (13 (12 () ()) (14 () ()))
(print (move-to-left-son t1)) ; (12 () ())
(print (current-element (move-to-left-son t1))) ; 12
(print (at-leaf? (move-to-right-son (move-to-left-son t1)))) ; #t
(print (insert-to-left 15 t1))  ; (13 (15 (12 () ()) ()) (14 () ()))