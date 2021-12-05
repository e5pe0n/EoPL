(define print
  (lambda (x)
    (for-each display `(,x "\n"))
  )
)

(define product
  (lambda (sos1 sos2)
    (define _product
      (lambda (_sos1 _sos2)
        (if (null? _sos1)
          '()
          (if (null? _sos2)
            (_product (cdr _sos1) sos2)
            (cons
              (list (car _sos1) (car _sos2))
              (_product _sos1 (cdr _sos2))
            )
          )
        )
      )
    )
    (_product sos1 sos2)
  )
)

(print
  (product '(a b c) '(x y))
) ; ((a x) (a y) (b x) (b y) (c x) (c y))