#lang eopl

(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))


(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)
  )
  (extend-tenv-with-self-and-super
    (self type?)
    (super-name symbol?)
    (tenv type-environment?)
  )
)

(define init-tenv
  (lambda () (empty-tenv))
)
(define apply-tenv
  (lambda (tenv search-name)
    (cases type-environment tenv
      (empty-tenv ()
        (eopl:error 'apply-tenv "no type found for ~s" search-name)
      )
      (extend-tenv (b-vars types saved-tenv)
        (cond
          ((position (lambda (x) (eqv? x search-name)) b-vars)
            => (lambda (n) (list-ref types n))
          )
          (else (apply-tenv saved-tenv search-name))
        )
      )
      (extend-tenv-with-self-and-super (self-name super-name saved-tenv)
        (case search-name
          ((%self) self-name)
          ((%super) super-name)
          (else (apply-tenv saved-tenv search-name))
        )
      )
    )
  )
)
