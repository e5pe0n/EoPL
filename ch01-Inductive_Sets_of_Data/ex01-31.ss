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

(print
  (interior-node 'baz
    (interior-node 'bar
      (leaf 1)
      (interior-node 'foo (leaf 1) (leaf 2))
    )
    (interior-node 'biz (leaf 4) (leaf 5))
  )
) ; (baz (bar 1 (foo 1 2)) (biz 4 5))
(print
  (leaf? (leaf 1))
) ; #t
(print
  (leaf? (interior-node 'baz (leaf 1) (leaf 2)))
) ; #f

(print
  (contents-of (interior-node 'baz (leaf 1) (leaf 2)))
) ; (baz 1 2)
(print
  (contents-of (leaf 1))
) ; 1
(print
  (lson
    (interior-node 'baz
      (interior-node 'bar
        (leaf 1)
        (interior-node 'foo (leaf 1) (leaf 2))
      )
      (interior-node 'biz (leaf 4) (leaf 5))
    )
  )
) ; (bar 1 (foo 1 2))
(print
  (lson
    (interior-node 'baz
      (leaf 1)
      (interior-node 'biz (leaf 4) (leaf 5))
    )
  )
) ; 1
(print
  (rson
    (interior-node 'baz
      (interior-node 'bar
        (leaf 1)
        (interior-node 'foo (leaf 1) (leaf 2))
      )
      (interior-node 'biz (leaf 4) (leaf 5))
    )
  )
) ; (biz 4 5)
(print
  (rson
    (interior-node 'baz
      (interior-node 'bar
        (leaf 1)
        (interior-node 'foo (leaf 1) (leaf 2))
      )
      (leaf 5)
    )
  )
) ; 5
(print
  (lson
    (rson
      (lson
        (interior-node 'baz
          (interior-node 'bar
            (leaf 1)
            (interior-node 'foo (leaf 1) (leaf 2))
          )
          (interior-node 'biz (leaf 4) (leaf 5))
        )
      )
    )
  )
) ; 1