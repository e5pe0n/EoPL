(import datatype)


(define all
  (lambda (f lst)
    (if (null? lst)
      #t
      (and (f (car lst)) (all f (cdr lst)))
    )
  )
)
(define red-blue-subtrees?
  (lambda (subtrees)
    (and (list? subtrees) (all red-blue-subtree? subtrees))
  )
)
(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
    (left red-blue-subtree?)
    (right red-blue-subtree?)
  )
  (blue-node
    (subtrees red-blue-subtrees?)
  )
  (leaf-node
    (num integer?)
  )
)
(define-datatype red-blue-tree red-blue-tree?
  (rbst
    (subtree red-blue-subtree?)
  )
)

(define _mark-leaves-with-red-depth
  (lambda (num-reds st)
    (cases red-blue-subtree st
      (red-node (left right)
        (red-node
          (_mark-leaves-with-red-depth (+ 1 num-reds) left)
          (_mark-leaves-with-red-depth (+ 1 num-reds) right)
        )
      )
      (blue-node (subtrees)
        (blue-node
          (let f ([lst subtrees])
            (if (null? lst)
              '()
              (cons (_mark-leaves-with-red-depth num-reds (car lst)) (f (cdr lst)))
            )
          )
        )
      )
      (leaf-node (num)
        (leaf-node num-reds)
      )
    )
  )
)
(define mark-leaves-with-red-depth
  (lambda (tree)
    (cases red-blue-tree tree
      (rbst (subtree)
        (rbst (_mark-leaves-with-red-depth 0 subtree))
      )
    )
  )
)
(define red-blue-subtree-to-list
  (lambda (st)
    (cases red-blue-subtree st
      (red-node (left right)
        (list
          'red
          (red-blue-subtree-to-list left)
          (red-blue-subtree-to-list right)
        )
      )
      (blue-node (subtrees)
        (list
          'blue
          (let f ([lst subtrees])
            (if (null? lst)
              '()
              (cons (red-blue-subtree-to-list (car lst)) (f (cdr lst)))
            )
          )
        )
      )
      (leaf-node (num) num)
    )
  )
)
(define red-blue-tree-to-list
  (lambda (tree)
    (cases red-blue-tree tree
      (rbst (subtree)
        (red-blue-subtree-to-list subtree)
      )
    )
  )
)


(define t1
  (rbst
    (red-node
      (blue-node
        (list
          (leaf-node 26)
          (leaf-node 12)
        )
      )
      (red-node
        (leaf-node 11)
        (blue-node
          (list
            (leaf-node 117)
            (leaf-node 14)
          )
        )
      )
    )
  )
)
(print
  (red-blue-tree-to-list
    (mark-leaves-with-red-depth t1)
  )
) ; (red (blue (1 1)) (red 2 (blue (2 2))))