(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

; NodeInSequence ::= (Int Listof(Int) Listof(Int))

(define number->sequence
  (lambda (n)
    (list n '() '())
  )
)
(define current-element
  (lambda (node)
    (car node)
  )
)
(define preceding
  (lambda (node)
    (cadr node)
  )
)
(define after
  (lambda (node)
    (caddr node)
  )
)
(define move-to-left
  (lambda (node)
    (list
      (car (preceding node))
      (cdr (preceding node))
      (cons (current-element node) (after node))
    )
  )
)
(define move-to-right
  (lambda (node)
    (list
      (car (after node))
      (cons (current-element node) (preceding node))
      (cdr (after node))
    )
  )
)
(define insert-to-left
  (lambda (n node)
    (list
      (current-element node)
      (cons n (preceding node))
      (after node)
    )
  )
)
(define insert-to-right
  (lambda (n node)
    (list
      (current-element node)
      (preceding node)
      (cons n (after node))
    )
  )
)
(define at-left-end?
  (lambda (node)
    (null? (preceding node))
  )
)
(define at-right-end?
  (lambda (node)
    (null? (after node))
  )
)

(define node1 '(6 (5 4 3 2 1) (7 8 9)))
(print (move-to-left node1))
(print (move-to-right node1))
(print (insert-to-left 13 node1))
(print (insert-to-right 13 node1))