(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

; BinTree ::= Int | (Symbol BinTree BinTree)
; (baz
;   (bar 1 (foo 1 2))
;   (biz 4 5)
; )

(define leaf
  (lambda (n)
    n
  )
)
(define interior-node
  (lambda (s left right)
    (list s left right)
  )
)
(define leaf?
  (lambda (node)
    (number? node)
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

(define lson
  (lambda (node)
    (contents-of (left node))
  )
)
(define rson
  (lambda (node)
    (contents-of (right node))
  )
)
(define contents-of
  (lambda (node)
    node
  )
)

(define mark-leaves-with-red-depth
  (lambda (bt)
    (let f ([root bt] [n 0])
      (if (leaf? root)
        (leaf n)
        (let
          (
            [m (if (eqv? (car root) 'red) (+ n 1) n)]
          )
          (interior-node (car root)
            (f (lson root) m)
            (f (rson root) m)
          )
        )
      )
    )
  )
)


(define bt1
  (interior-node 'red
    (interior-node 'bar (leaf 26) (leaf 12))
    (interior-node 'red
      (leaf 11)
      (interior-node 'quux (leaf 117) (leaf 14))
    )
  )
)

(print
  (mark-leaves-with-red-depth bt1)
) ; (red (bar 1 1) (red 2 (quux 2 2)))