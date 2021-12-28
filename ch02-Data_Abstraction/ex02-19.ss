(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define number->bintree
  (lambda (n)
    (list (list n '() '() #f) '())
  )
)
(define bt-node
  (lambda (node)
    (car node)
  )
)
(define is-left-son?
  (lambda (bnode)
    (cadddr bnode)
  )
)
(define left-son
  (lambda (bnode)
    (cadr bnode)
  )
)
(define right-son
  (lambda (bnode)
    (caddr bnode)
  )
)
(define current-element
  (lambda (node)
    (car (bt-node node))
  )
)
(define preceding
  (lambda (node)
    (cadr node)
  )
)
(define move-to-left-son
  (lambda (node)
    (list
      (left-son (bt-node node))
      (cons (bt-node node) (cadr node))
    )
  )
)
(define move-to-right-son
  (lambda (node)
    (list
      (right-son (bt-node node))
      (cons (bt-node node) (cadr node))
    )
  )
)
(define move-up
  (lambda (node)
    (list
      (let ([curr-bn (bt-node node)] [next-bn (car (cadr node))])
        (list
          (car next-bn)
          (if (is-left-son? curr-bn) curr-bn (left-son next-bn))
          (if (is-left-son? curr-bn) (right-son next-bn) curr-bn)
          (is-left-son? next-bn)
        )
      )
      (cdr (cadr node))
    )
  )
)
(define insert-to-left
  (lambda (n node)
    (list
      (list
        (current-element node)
        (list n (bt-node (move-to-left-son node)) '() #t)
        (bt-node (move-to-right-son node))
        (is-left-son? (bt-node node))
      )
      (cadr node)
    )
  )
)
(define insert-to-right
  (lambda (n node)
    (list
      (list
        (current-element node)
        (bt-node (move-to-left-son node))
        (list n (bt-node (move-to-right-son node)) '() #f)
        (is-left-son? (bt-node node))
      )
      (cadr node)
    )
  )
)
(define at-leaf?
  (lambda (node)
    (null? (bt-node node))
  )
)
(define at-root?
  (lambda (node)
    (null? (cadr node))
  )
)

(define t1
  (insert-to-right 14
    (insert-to-left 12
      (number->bintree 13)
    )
  )
)
; (print t1)
; (print (move-to-left-son t1))
; (print (current-element (move-to-left-son t1)))
; (print (at-leaf? (move-to-right-son (move-to-left-son t1))))
; (print (insert-to-left 15 t1))

(let
  (
    [t2
      (move-up
        (move-up
          (move-to-right-son
            (insert-to-right 16
              (move-to-left-son
                (insert-to-left 15 t1)
              )
            )
          )
        )
      )
    ]
  )
  (print t2)  ; ((13 (15 (12 () () #t) (16 () () #f) #t) (14 () () #f) #f) ())
  (print (at-root? t2)) ; #t
  (print (current-element t2))  ; 13
)
