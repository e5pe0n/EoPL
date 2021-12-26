; A simple binary tree with numbers:

(import datatype)

(define-datatype tree tree?
  (leaf (n number?))
  (branch (left tree?) (right tree?)) )

(define t (branch (branch (leaf 33) (leaf 44)) (leaf 55)))

(define (listify t)
  (cases tree t
    (leaf (n) n)
    (branch (left right) (cons (listify left) (listify right))) ) )

(print (listify t))