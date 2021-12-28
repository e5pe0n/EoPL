(import datatype)

(define-datatype bintree bintree?
  (leaf-node
    (num integer?)
  )
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)
  )
)
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right)
        (list 'interior-node key left right)
      )
    )
  )
)
(define _max-interior
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num) #f)
      (interior-node (key left right)
        (cases bintree left
          (leaf-node (l-num)
            (cases bintree right
              (leaf-node (r-num)
                (cons key (+ l-num r-num))
              )
              (interior-node (k lc rc)
                (let*
                  (
                    [r-res (_max-interior right)]
                    [sum (+ l-num (cdr r-res))]
                  )
                  (if (< sum (cdr r-res))
                    r-res
                    (cons key sum)
                  )
                )
              )
            )
          )
          (interior-node (lk llc lrc)
            (let ([l-res (_max-interior left)])
              (cases bintree right
                (leaf-node (r-num)
                  (cons key (+ (cdr l-res) r-num))
                )
                (interior-node (rk rlc rrc)
                  (let*
                    (
                      [r-res (_max-interior right)]
                      [sum (+ (cdr l-res) (cdr r-res))]
                    )
                    (cond
                      (
                        (and (< (cdr l-res) sum) (< (cdr r-res) sum))
                        (cons key sum)
                      )
                      (
                        (and (< (cdr r-res) (cdr l-res)) (< sum (cdr l-res)))
                        l-res
                      )
                      (else r-res)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
(define max-interior
  (lambda (bt)
    (car (_max-interior bt))
  )
)

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3))
)
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1)
)
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1))
)

(print (max-interior tree-2)) ; foo
(print (max-interior tree-3)) ; baz
