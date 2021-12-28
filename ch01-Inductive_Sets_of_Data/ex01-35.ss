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

(define cnt-leaves
  (lambda (node n)
    (if (leaf? node)
      (cons (leaf n) (+ n 1))
      (let* (
          [left-res (cnt-leaves (lson node) n)]
          [right-res (cnt-leaves (rson node) (cdr left-res))]
        )
        (cons
          (interior-node (car node)
            (car left-res)
            (car right-res)
          )
          (cdr right-res)
        )
      )
    )
  )
)

(define number-leaves
  (lambda (bst)
    (car (cnt-leaves bst 0))
  )
)

(define bst1
  (interior-node 'foo
    (interior-node 'bar
      (leaf 26)
      (leaf 12)
    )
    (interior-node 'baz
      (leaf 11)
      (interior-node 'quux
        (leaf 117)
        (leaf 14)
      )
    )
  )
)

(print
  (number-leaves bst1)
) ; (foo (bar 0 1) (baz 2 (quux 3 4)))