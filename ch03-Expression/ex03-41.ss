(import error-utils)
(import datatype)

; Senv = Listof(Sym)
; Lexaddr = (N . N)

; () -> Senv
(define empty-senv
  (lambda ()
    '()
  )
)
; Var x Senv -> Senv
(define extend-senv
  (lambda (vars senv)
    (cons vars senv)
  )
)

; List x SchemeVal -> Int
(define list-index
  (lambda (list1 x)
    (let f ([i 0] [lst1 list1])
      (if (null? lst1)
        -1
        (if (eqv? x (car lst1))
          i
          (f (+ 1 i) (cdr lst1))
        )
      )
    )
  )
)

; Senv x Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      (
        (memv var (car senv))
        (cons 0 (list-index (car senv) var))
      )
      (else
        (let ([res (apply-senv (cdr senv) vars-name var)])
          (cons (+ 1 (car res)) (cdr res))
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

; (SchemeVal -> Bool) -> List -> Bool
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
    (vars id-list?)
    (exps exp-list?)
    (body expression?)
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
    (vars id-list?)
    (body expression?)
  )
  (call-exp
    (rator expression?)
    (rands exp-list?)
  )
  (nameless-var-exp
    (addr num-pair?)
  )
  (nameless-unpack-exp
    (exp1 expression?)
    (body expression?)
  )
  (nameless-let-exp
    (exps exp-list?)
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
(define num-pair?
  (lambda (x)
    (and (pair? x) (number? (car x)) (number? (cdr x)))
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
          (map
            (lambda (x)
              (list (translation-of (car x) senv) (translation-of (cadr x)))
            )
            exp-pairs
          )
        )
      )
      (emptylist-exp () (emptylist-exp))
      (list-exp (list1)
        (list-exp (map (lambda (x) (translation-of x senv)) list1))
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
          (translation-of body (extend-senv vars senv))
        )
      )
      (var-exp (var)
        (nameless-var-exp
          (apply-senv senv var)
        )
      )
      (let-exp (vars exps body)
        (nameless-let-exp
          (map (lambda (x) (translation-of x senv)) exps)
          (translation-of body (extend-senv vars senv))
        )
      )
      (proc-exp (vars body)
        (nameless-proc-exp
          (translation-of body (extend-senv vars senv))
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
; ListOf(ExpVal) x Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (vals nameless-env)
    (cons vals nameless-env)
  )
)
; Nameless-env x Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env addr)
    (list-ref
      (list-ref nameless-env (car addr))
      (cdr addr)
    )
  )
)
; Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (body saved-nameless-env)
        (value-of body
          (extend-nameless-env vals saved-nameless-env)
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
          (map (lambda (x) (expval->any (value-of x nameless-env))) list1)
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
      (call-exp (rator rands)
        (let
          (
            [proc1 (expval->proc (value-of rator nameless-env))]
            [vals (map (lambda (x) (value-of x nameless-env)) rands)]
          )
          (apply-procedure proc1 vals)
        )
      )
      (nameless-var-exp (addr)
        (apply-nameless-env nameless-env addr)
      )
      (nameless-let-exp (exps body)
        (value-of body
          (extend-nameless-env
            (map (lambda (x) (value-of x nameless-env)) exps)
            nameless-env
          )
        )
      )
      (nameless-unpack-exp (exp1 body)
        (value-of body
          (let ([list1 (expval->list (value-of exp1 nameless-env))])
            (extend-nameless-env
              (map (lambda (x) (any->expval x)) list1)
              nameless-env
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
  (let-exp (list 'x) (list (const-exp 30))
    (let-exp (list 'x 'y) (list
        (diff-exp (var-exp 'x) (const-exp 1))
        (diff-exp (var-exp 'x) (const-exp 2))
      )
      (diff-exp (var-exp 'x) (var-exp 'y))
    )
  )
))  ; 1
(print (_f
  (let-exp (list 'f) (list (proc-exp (list 'x 'y) (diff-exp (var-exp 'x) (var-exp 'y))))
    (call-exp (var-exp 'f) (list (const-exp 1) (const-exp 2)))
  )
))  ; -1