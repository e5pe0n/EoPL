(import error-utils)
(import datatype)

; Senv = Listof(Sym)
; Lexaddr = N

; () -> Senv
(define empty-senv
  (lambda ()
    '()
  )
)
; Var x Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)
  )
)
; Senv x Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv)) 0)
      (else (+ 1 (apply-senv (cdr senv) var)))
    )
  )
)

(define trim-senv
  (lambda (var senv)
    (cond
      ((null? senv) (empty-senv))
      ((eqv? var (car senv)) (cdr senv))
      (else (cons (car senv) (trim-senv var (cdr senv))))
    )
  )
)

(define report-unbound-var
  (lambda (var)
    (errorf "unbound-var: ~s" var)
  )
)

(define-datatype program program?
  (a-program
    (exp1 expression?)
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
  (nameless-var-exp
    (num number?)
  )
  (nameless-let-exp
    (exp1 expression?)
    (body expression?)
  )
  (nameless-proc-exp
    (body expression?)
  )
)
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)
  )
)

; Exp x Senv -> Nameless-exp
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
          (translation-of body (extend-senv var senv))
        )
      )
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body (extend-senv var (trim-senv var senv)))
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
  (lambda ()
    (empty-senv)
  )
)
; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program
      (translation-of-program
        (scan&parse string)
      )
    )
  )
)

; (SchemeVal -> Bool) -> List -> Bool
(define list-of
  (lambda (pred)
    (lambda (lst)
      (if (null? lst)
        #t
        (and
          (pred (car lst))
          ((list-of pred) (cdr lst))
        )
      )
    )
  )
)

; SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)
  )
)
; () -> Nameless-env
(define empty-nameless-env
  (lambda ()
    '()
  )
)
; ExpVal x Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)
  )
)
; Nameless-env x Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)
  )
)
; Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
        (value-of body
          (extend-nameless-env val saved-nameless-env)
        )
      )
    )
  )
)

(define-datatype expval expval?
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
  )
  (proc-val
    (proc1 proc?)
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
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'num val)
    )
  )
)
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
    )
  )
)
(define report-expval-extractor-error
  (lambda (name val)
    (errorf "~s extractor cannot extract ~s" name val)
  )
)

; Nameless-exp x Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
        (num-val
          (- (expval->num (value-of exp1 nameless-env))
            (expval->num (value-of exp2 nameless-env))
          )
        )
      )
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 nameless-env))))
      )
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 nameless-env))
          (value-of exp2 nameless-env)
          (value-of exp3 nameless-env)
        )
      )
      (call-exp (rator rand)
        (let
          (
            [proc1 (expval->proc (value-of rator nameless-env))]
            [val (value-of rand nameless-env)]
          )
          (apply-procedure proc1 val)
        )
      )
      (nameless-var-exp (n)
        (apply-nameless-env nameless-env n)
      )
      (nameless-let-exp (exp1 body)
        (let ([val (value-of exp1 nameless-env)])
          (value-of body
            (extend-nameless-env val nameless-env)
          )
        )
      )
      (nameless-proc-exp (body)
        (proc-val
          (procedure body nameless-env)
        )
      )
      (else (report-invalid-translated-expression exp))
    )
  )
)
(define report-invalid-translated-expression
  (lambda (exp)
    (errorf "invalid translated expression: ~s" exp)
  )
)

(define init-nameless-env
  (lambda ()
    (empty-nameless-env)
  )
)

; Nameless-program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-nameless-env))
      )
    )
  )
)

(define _f
  (lambda (e)
    (expval->num
      (value-of-program
        (translation-of-program
          (a-program e)
        )
      )
    )
  )
)

(print (_f
  (let-exp 'x (const-exp 1)
    (let-exp 'y (const-exp 2)
      (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (var-exp 'y)))
        (let-exp 'y (const-exp 4)
          (call-exp (var-exp 'f) (var-exp 'y))
        )
      )
    )
  )
)) ; 2