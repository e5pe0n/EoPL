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
  (extend-env-with-self-and-super
    (self object?)
    (super-name symbol?)
    (saved-env environment?)
  )
)

(define env->list
  (lambda (env)
    (cases environment env
      (empty-env () '())
      (extend-env (sym val saved-env)
        (cons
          (list sym val)
          (env->list saved-env)
        )
      )
      (extend-env-rec** (p-names b-varss p-bodies saved-env)
        (cons
          (list 'letrec p-names '...)
          (env->list saved-env)
        )
      )
      (extend-env-with-self-and-super (self super-name saved-env)
        (cons
          (list 'self self 'super super-name)
          (env->list saved-env)
        )
      )
    )
  )
)

(define expval->printable
  (lambda (val)
    (cases expval val
      (proc-val (p)
        (cases proc p
          (procedure (var body saved-env)
            (list 'procedure var '... (env->list saved-env))
          )
        )
      )
      (else val)
    )
  )
)