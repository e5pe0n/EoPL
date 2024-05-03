#lang eopl

(require "lang.rkt")

(provide (all-defined-out))

(define the-store 'uninitialized)
(define empty-store
  (lambda () '())
)
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))
  )
)
(define get-store
  (lambda () the-store)
)

(define reference?
  (lambda  (v) (integer? v))
)
(define newref
  (lambda (val)
    (let ([next-ref (length the-store)])
      (set! the-store (append the-store (list val)))
      next-ref
    )
  )
)
(define deref
  (lambda (ref) (list-ref the-store ref))
)
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec (
          [setref-inner
            (lambda (store1 ref1)
              (cond
                ((null? store1) (report-invalid-reference ref the-store))
                ((zero? ref1) (cons val (cdr store1)))
                (else
                  (cons
                    (car store1)
                    (setref-inner (cdr store1) (- ref1 1))
                  )
                )
              )
            )
          ]
        )
        (setref-inner the-store ref)
      )
    )
  )
)
(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store)
  )
)
(define get-store-as-list
  (lambda ()
    (letrec (
        [inner-loop
          (lambda (sto n)
            (if (null? sto)
              '()
              (cons
                (list n (car sto))
                (inner-loop (cdr sto) (+ n 1))
              )
            )
          )
        ]
      )
      (inner-loop the-store 0)
    )
  )
)
