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
          (translation-of body (extend-senv vars #f senv))
        )
      )
      (var-exp (var)
        (let ([res (apply-senv senv var)])
          (let ([addr (car res)] [bound (cdr res)])
            (if bound
              (nameless-letrec-var-exp (car addr) (cdr addr))
              (nameless-var-exp (car addr) (cdr addr))
            )
          )
        )
      )
      (let-exp (vars exps body)
        (nameless-let-exp
          (map (lambda (x) (translation-of x senv)) exps)
          (translation-of body
            (extend-senv vars #f senv)
          )
        )
      )
      (letrec-exp (p-names b-vars-list p-bodys letrec-body)
        (nameless-letrec-exp
          (map (lambda (p-name b-vars p-body) (translation-of p-body (extend-senv b-vars #f (extend-senv p-names #t senv)))) p-names b-vars-list p-bodys)
          (translation-of letrec-body (extend-senv p-names #t senv))
        )
      )
      (proc-exp (vars body)
        (nameless-proc-exp
          (translation-of body
            (extend-senv vars #f senv)
          )
        )
      )
      (call-exp (rator rands)
        (call-exp
          (translation-of rator senv)
          (map (lambda (x) (translation-of x senv)) rands)
        )
      )
      (else (report-invalid-source-expression exp))
    )
  )
)