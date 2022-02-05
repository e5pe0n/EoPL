(import error-utils)

(define empty-senv
  (lambda ()
    '()
  )
)
(define extend-senv
  (lambda (var senv)
    (cons var senv)
  )
)
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv)) 0)
      (else (+ 1 (apply-senv (cdr senv) var)))
    )
  )
)

(define report-unbound-var
  (lambda (var)
    (errorf "unbound-var: ~s" var)
  )
)

(define-datatype expression expression?
  (const-exp
    (num number?)
  )
  (diff-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (zero?-exp
    (exp1 expression?)
  )
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?)
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?)
  )
  (proc-exp
    (var identifier?)
    (body expression?)
  )
  (call-exp
    (rator expression?)
    (rand expression?)
  )
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

(define proc?
  (lambda (val)
    (procedure? val)
  )
)

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
          (translation-of exp1 senv1)
        )
      )
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of ex1 senv)
          (translation-of ex2 senv)
          (translation-of ex3 senv)
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
          (translation-of body (exnted-senv var senv))
        )
      )
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body (extend-senv var senv))
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

(define report-invalid-source-expression
  (lambda (exp)
    (errorf "invalid source expression: ~s" exp)
  )
)