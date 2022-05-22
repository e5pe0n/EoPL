#lang eopl

(require "utils.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype expval expval?
  (num-val
    (num number?)
  )
)
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'num val)
    )
  )
)
