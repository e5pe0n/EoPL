(import error-utils)
(import datatype)

; Senv = Listof(PairOf(Sym, Bool))
; Lexaddr = N

; () -> Senv
(define empty-senv
  (lambda ()
    '()
  )
)
; Var x Bool  x Senv -> Senv
(define extend-senv
  (lambda (var letrec-bound senv)
    (cons (cons var letrec-bound) senv)
  )
)
; Senv x Var -> Lexaddr x Bool
(define apply-senv
  (lambda (senv var)
    (let ([head (car senv)])
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car head)) (cons 0 (cdr head)))
        (else
          (let ([res (apply-senv (cdr senv) var)])
            (cons (+ 1 (car res)) (cdr res))
          )
        )
      )
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

; (SchemeVal -> Bool) x List -> Bool
(define list-of
  (lambda (pred)
    (lambda (list1)
      (if (null? list1)
        #t
        (and
          (pred (car list1))
          ((list-of pred) (cdr list1))
        )
      )
    )
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
  (cond-exp
    (exp-pairs (list-of exp-list?))
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?)
  )
  (letrec-exp
    (p-name identifier?)
    (b-var identifier?)
    (p-body expression?)
    (letrec-exp expression?)
  )
  (emptylist-exp)
  (list-exp
    (exps exp-list?)
  )
  (cons-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (unpack-exp
    (vars id-list?)
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
  (nameless-letrec-var-exp
    (num number?)
  )
  (nameless-unpack-exp
    (exp1 expression?)
    (body expression?)
  )
  (nameless-let-exp
    (exp1 expression?)
    (body expression?)
  )
  (nameless-letrec-exp
    (p-body expression?)
    (letrec-body expression?)
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
(define id-list?
  (lambda (list1)
    ((list-of identifier?) list1)
  )
)
(define exp-list?
  (lambda (list1)
    ((list-of expression?) list1)
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
          (translation-of exp1 senv)
          (translation-of exp2 senv)
          (translation-of exp3 senv)
        )
      )
      (cond-exp (exp-pairs)
        (cond-exp
          (let f ([pairs exp-pairs])
            (if (null? pairs)
              '()
              (let ([pair (car pairs)])
                (cons
                  (list
                    (translation-of (car pair) senv)
                    (translation-of (cadr pair) senv)
                  )
                  (f (cdr pairs))
                )
              )
            )
          )
        )
      )
      (emptylist-exp () (emptylist-exp))
      (list-exp (list1)
        (list-exp
          (let f ([lst1 list1])
            (if (null? lst1)
              '()
              (cons (translation-of (car lst1) senv) (f (cdr lst1)))
            )
          )
        )
      )
      (cons-exp (exp1 exp2)
        (cons-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
        )
      )
      (unpack-exp (vars exp1 body)
        (nameless-unpack-exp
          (translation-of exp1 senv)
          (translation-of body
            (let f ([vs vars])
              (if (null? vs)
                senv
                (extend-senv (car vs) #f (f (cdr vs)))
              )
            )
          )
        )
      )
      (var-exp (var)
        (let ([res (apply-senv senv var)])
          (if (cdr res)
            (nameless-letrec-var-exp (car res))
            (nameless-var-exp (car res))
          )
        )
      )
      (let-exp (var exp1 body)
        (nameless-let-exp
          (translation-of exp1 senv)
          (translation-of body (extend-senv var #f senv))
        )
      )
      (letrec-exp (p-name b-var p-body letrec-body)
        (nameless-letrec-exp
          (translation-of p-body (extend-senv b-var #f (extend-senv p-name #t senv)))
          (translation-of letrec-body (extend-senv p-name #t senv))
        )
      )
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body (extend-senv var #f senv))
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
; Exp x Nameless-env
(define extend-nameless-env-rec
  (lambda (p-body nameless-env)
    (cons (proc-val (procedure p-body nameless-env)) nameless-env)
  )
)
; Nameless-env x Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)
  )
)
; Nameless-env x Lexaddr -> ExpVal
(define apply-nameless-env-rec
  (lambda (nameless-env n)
    (cases proc (expval->proc (list-ref nameless-env n))
      (procedure (body saved-nameless-env)
        (proc-val (procedure body (extend-nameless-env-rec body nameless-env)))
      )
    )
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
  (list-val
    (list1 list?)
  )
)
(define any->expval
  (lambda (x)
    (cond
      ((number? x) (num-val x))
      ((boolean? x) (bool-val x))
      ((list? x) (list-val x))
    )
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
      (else report-expval-extractor-error 'bool val)
    )
  )
)
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'proc val)
    )
  )
)
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (list1) list1)
      (else report-expval-extractor-error 'list val)
    )
  )
)
(define expval->any
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (proc-val (proc1) proc1)
      (list-val (list1) list1)
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
      (cond-exp (exp-pairs)
        (if (null? exp-pairs)
          (error "value-of cond-exp; no satisfied condition.")
          (let ([head (car exp-pairs)])
            (value-of
              (if (expval->bool (value-of (car head) nameless-env))
                (cadr head)
                (cond-exp (cdr exp-pairs))
              )
              nameless-env
            )
          )
        )
      )
      (emptylist-exp () (list-val '()))
      (list-exp (list1)
        (list-val
          (let f ([lst1 list1])
            (if (null? lst1)
              '()
              (cons
                (expval->any (value-of (car lst1) nameless-env))
                (f (cdr lst1))
              )
            )
          )
        )
      )
      (cons-exp (exp1 exp2)
        (list-val
          (cons
            (expval->any (value-of exp1 nameless-env))
            (expval->any (value-of exp2 nameless-env))
          )
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
      (nameless-letrec-var-exp (n)
        (apply-nameless-env-rec nameless-env n)
      )
      (nameless-let-exp (exp1 body)
        (value-of body
          (extend-nameless-env (value-of exp1 nameless-env) nameless-env)
        )
      )
      (nameless-letrec-exp (p-body letrec-body)
        (value-of letrec-body
          (extend-nameless-env-rec p-body nameless-env)
        )
      )
      (nameless-unpack-exp (exp1 body)
        (value-of body
          (let f ([list1 (expval->list (value-of exp1 nameless-env))])
            (if (null? list1)
              nameless-env
              (extend-nameless-env (any->expval (car list1)) (f (cdr list1)))
            )
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
    (expval->any
      (value-of-program
        (translation-of-program
          (a-program e)
        )
      )
    )
  )
)
(print (_f
  (let-exp 'x (const-exp 3)
    (letrec-exp 'double 'x
      (if-exp (zero?-exp (var-exp 'x))
        (const-exp 0)
        (diff-exp
          (call-exp
            (var-exp 'double)
            (diff-exp
              (var-exp 'x)
              (const-exp 1)
            )
          )
          (const-exp -2)
        )
      )
      (let-exp 'y (const-exp 6)
        (diff-exp
          (call-exp (var-exp 'double) (var-exp 'x))
          (var-exp 'y)
        )
      )
    )
  )
))  ; 0
