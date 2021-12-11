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

(define double-tree
  (lambda (bt)
    (let ([root (contents-of bt)])
      (if (leaf? root)
        (leaf (* 2 root))
        (interior-node (car root)
          (double-tree (lson root))
          (double-tree (rson root))
        )
      )
    )
  )
)
(define bt1
  (interior-node 'baz
    (interior-node 'bar
      (leaf 1)
      (interior-node 'foo (leaf 1) (leaf 2))
    )
    (interior-node 'biz (leaf 4) (leaf 5))
  )
)
(print
  (double-tree bt1)
) ; (baz (bar 2 (foo 2 4)) (biz 8 10))