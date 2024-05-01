#lang eopl

(require "lang.rkt")
(require "store.rkt")
(require "classes.rkt")

(provide (all-defined-out))

(define-datatype expval expval?
  (num-val (value number?))
  (bool-val (boolean boolean?))
  (proc-val (proc proc?))
  (ref-val (ref reference?))
  (obj-val (obj object?))
  (list-val (lst (list-of expval?)))
)

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v))
    )
  )
)
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v))
    )
  )
)
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v))
    )
  )
)
(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else (expval-extractor-error 'ref v))
    )
  )
)
(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v))
    )
  )
)


(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)
  )
)


(define-datatype proc proc?
  (procedure
    (vars (list-of symbol?))
    (body expression?)
    (env environment?)
  )
)

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (b-vars (list-of symbol?))
    (b-vals (list-of reference?))
    (saved-env environment?)
  )
  (extend-env-rec**
    (proc-names (list-of symbol?))
    (b-varss (list-of (list-of symbol?)))
    (proc-bodies (list-of expression?))
    (saved-env environment?)
  )
  (extend-env-with-self-and-super-and-host
    (self object?)
    (super-name symbol?)
    (host-name symbol?)
    (saved-env environment?)
  )
)
