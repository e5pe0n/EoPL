#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")
(require "errors.rkt")

(provide (all-defined-out))

; Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (a-program
          (translation-of exp1 (init-senv))
        )
      )
    )
  )
)

; () -> Senv
(define init-senv
  (lambda () '())
)

; Exp -> Senv -> Exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (diff-exp (exp1 exp2)
        (diff-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
        )
      )
      (zero?-exp (exp1)
        (zero?-exp
          (translation-of exp1 senv)
        )
      )
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
          (translation-of exp3 senv)
        )
      )
      (cond-exp (lhs-list rhs-list)
        (cond-exp
          (map (lambda (x) (translation-of x senv)) lhs-list)
          (map (lambda (x) (translation-of x senv)) rhs-list)
        )
      )
      (cons-exp (exp1 exp2)
        (cons-exp
          (translation-of exp1 senv) (translation-of exp2 senv)
        )
      )
      (emptylist-exp () (emptylist-exp))
      (unpack-exp (vars exp1 body)
        (nameless-unpack-exp
          (translation-of exp1 senv)
          (translation-of body
            (let f ([vs vars])
              (if (null? vs)
                senv
                (extend-senv (car vs) (f (cdr vs)))
              )
            )
          )
        )
      )
      (var-exp (var)
        (nameless-var-exp
          (apply-senv senv var)
        )
      )
      (let-exp (var exp1 body)
        (nameless-let-exp
          (translation-of exp1 senv)
          (translation-of body
            (extend-senv var senv)
          )
        )
      )
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body
            (extend-senv var senv)
          )
        )
      )
      (call-exp (rator rand)
        (call-exp
          (translation-of rator senv)
          (translation-of rand senv)
        )
      )
      (else (report-invalid-source-expression exp))
    )
  )
)