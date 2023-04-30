#lang eopl

(require "data-structures.rkt")
(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Answer = Type

(define-datatype answer answer?
  (an-answer
    (ty type?)
  )
)

; Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (begin
          (init-sn!)
          (init-subst!)
          (init-sns!)
          (init-last-subst!)
          (init-next-subst-idx!)
          (cases answer (type-of exp1 (init-tenv))
            (an-answer (ty)
              (apply-subst-to-type ty)
            )
          )
        )
      )
    )
  )
)

; Exp * Tenv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (an-answer (int-type)))
      (var-exp (var) (an-answer (apply-tenv tenv var)))
      (diff-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv)
          (an-answer (ty1)
            (begin
              (unifier ty1 (int-type) exp1)
              (cases answer (type-of exp2 tenv)
                (an-answer (ty2)
                  (begin
                    (unifier ty2 (int-type) exp2)
                    (an-answer (int-type))
                  )
                )
              )
            )
          )
        )
      )
      (zero?-exp (exp1)
        (cases answer (type-of exp1 tenv)
          (an-answer (ty1)
            (begin
              (unifier ty1 (int-type) exp)
              (an-answer (bool-type))
            )
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (cases answer (type-of exp1 tenv)
          (an-answer (ty1)
            (begin
              (unifier ty1 (bool-type) exp1)
              (cases answer (type-of exp2 tenv)
                (an-answer (ty2)
                  (cases answer (type-of exp3 tenv)
                    (an-answer (ty3)
                      (begin
                        (unifier ty2 ty3 exp)
                        (an-answer ty2)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      (let-exp (var exp1 body)
        (cases answer (type-of exp1 tenv)
          (an-answer (exp1-type)
            (type-of body (extend-tenv var exp1-type tenv))
          )
        )
      )
      (proc-exp (var otype body)
        (let ([var-type (otype->type otype)])
          (cases answer (type-of body (extend-tenv var var-type tenv))
            (an-answer (body-type)
              (an-answer (proc-type var-type body-type))
            )
          )
        )
      )
      (call-exp (rator rand)
        (let ([result-type (fresh-tvar-type)])
          (cases answer (type-of rator tenv)
            (an-answer (rator-type)
              (cases answer (type-of rand tenv)
                (an-answer (rand-type)
                  (begin
                    (unifier rator-type (proc-type rand-type result-type) exp)
                    (an-answer result-type)
                  )
                )
              )
            )
          )
        )
      )
      (letrec-exp (p-result-otype p-name b-var b-var-otype p-body letrec-body)
        (let (
            [p-result-type (otype->type p-result-otype)]
            [b-var-type (otype->type b-var-otype)]
          )
          (let ([tenv-for-letrec-body (extend-tenv p-name (proc-type b-var-type p-result-type) tenv)])
            (cases answer (type-of p-body (extend-tenv b-var b-var-type tenv-for-letrec-body))
              (an-answer (p-body-type)
                (begin
                  (unifier p-body-type p-result-type p-body)
                  (type-of letrec-body tenv-for-letrec-body)
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-datatype t-environment t-environment?
  (empty-tenv)
  (extend-tenv
    (saved-var symbol?)
    (saved-ty type?)
    (saved-tenv t-environment?)
  )
)

; () -> TEnv
(define init-tenv
  (lambda () (empty-tenv))
)

; TEnv * Var -> Type
(define apply-tenv
  (lambda (tenv search-var)
    (cases t-environment tenv
      (empty-tenv () (report-no-binding-found 'apply-tenv search-var))
      (extend-tenv (saved-var saved-ty saved-tenv)
        (if (eqv? search-var saved-var)
          saved-ty
          (apply-tenv saved-tenv search-var)
        )
      )
    )
  )
)

; Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)
      0
    )
  )
)

; Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
      "Types didn't match: ~s != ~s in~%~s"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp
    )
  )
)

; Type * Exp -> Unspecified
(define report-rator-not-a-proc-type
  (lambda (rator-ty rator-exp)
    (eopl:error 'call-exp
      "rator must be a proc type;~%  rator-ty=~a~%  rator-exp=~a"
      (type-to-external-form rator-ty)
      (type-to-external-form rator-exp)
      exp
    )
  )
)


; Type * Tvar * Type -> Type
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
        (proc-type
          (apply-one-subst arg-type tvar ty1)
          (apply-one-subst result-type tvar ty1)
        )
      )
      (tvar-type (sn)
        (if (equal? ty0 tvar) ty1 ty0)
      )
    )
  )
)

; Type * Subst -> Type
(define apply-subst-to-type
  (lambda (ty)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (t1 t2)
        (proc-type
          (apply-subst-to-type t1)
          (apply-subst-to-type t2)
        )
      )
      (tvar-type (sn)
        (let ([subst-idx (vector-ref the-sns sn)])
          (if (= subst-idx -1)
            ty
            (let ([t (vector-ref the-subst subst-idx)] [n (vector-ref the-last-subst sn)])
              (if (= n the-next-subst-idx)
                t
                (let loop ([i (+ subst-idx 1)] [t t])
                  (if (< i the-next-subst-idx)
                    (let ([rhs (vector-ref the-subst i)])
                      (loop (+ i 1) (apply-one-subst t (tvar-type i) rhs))
                    )
                    (begin
                      (vector-set! the-subst subst-idx t)
                      (vector-set! the-last-subst sn the-next-subst-idx)
                      t
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


; SchemeVal -> Bool
(define pair-of
  (lambda (pred1 pred2)
    (lambda (val)
      (and (pair? val) (pred1 (car val)) (pred2 (cdr val)))
    )
  )
)

; SchemeVal -> Bool
(define substitution?
  (vector-of (pair-of tvar-type? type?))
)

; Subst
(define the-subst 'uninitialized)
(define the-subst-size 100)

(define the-sns 'uninitialized)
(define the-sns-size the-subst-size)

(define the-last-subst 'uninitialized)
(define the-last-subst-size the-subst-size)

(define the-next-subst-idx 'uninitialized)

; () -> Unspecified
(define init-subst!
  (lambda ()
    (set! the-subst (make-vector the-subst-size (tvar-type -1)))
  )
)

; () -> Unspecified
(define init-sns!
  (lambda ()
    (set! the-sns (make-vector the-sns-size -1))
  )
)

; () -> Unspecified
(define init-last-subst!
  (lambda ()
    (set! the-last-subst (make-vector the-last-subst-size -1))
  )
)

; () -> Unspecified
(define init-next-subst-idx!
  (lambda ()
    (set! the-next-subst-idx 0)
  )
)

; Ref * Sto -> ()
(define report-invalid-tvar
  (lambda (tvar subst)
    (eopl:error 'invalid-tvar "invalid tvar: tvar=~s, subst=~s" tvar subst)
  )
)

; Tvar * Type -> Unspecified
(define extend-subst
  (lambda (tvar ty)
    (cases type tvar
      (tvar-type (sn)
        (vector-set! the-subst the-next-subst-idx ty)
        (vector-set! the-sns sn the-next-subst-idx)
        (vector-set! the-last-subst sn the-next-subst-idx)
        (set! the-next-subst-idx (+ 1 the-next-subst-idx))
      )
      (else (eopl:error 'invalid-tvar "tvar=~s is not tvar-type" tvar))
    )
  )
)

; Type * Type * Subst * Exp -> ()
(define unifier
  (lambda (ty1 ty2 exp)
    (let (
        [ty1 (apply-subst-to-type ty1)]
        [ty2 (apply-subst-to-type ty2)]
      )
      (cond
        ((equal? ty1 ty2) 23)
        ((tvar-type? ty1)
          (if (no-occurrence? ty1 ty2)
            (extend-subst ty1 ty2)
            (report-no-occurrence-violation ty1 ty2 exp)
          )
        )
        ((tvar-type? ty2)
          (if (no-occurrence? ty2 ty1)
            (extend-subst ty2 ty1)
            (report-no-occurrence-violation ty2 ty1 exp)
          )
        )
        ((and (proc-type? ty1) (proc-type? ty2))
          (begin
            (unifier
              (proc-type->arg-type ty1) (proc-type->arg-type ty2) exp
            )
            (unifier
              (proc-type->result-type ty1) (proc-type->result-type ty2) exp
            )
          )
        )
        (else (report-unification-failure ty1 ty2 exp))
      )
    )
  )
)


; Tvar * Type -> Bool
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-type result-type)
        (and
          (no-occurrence? tvar arg-type)
          (no-occurrence? tvar result-type)
        )
      )
      (tvar-type (sn)
        (not (equal? tvar ty))
      )
    )
  )
)

; Type * Type * Exp -> ()
(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (eopl:error 'no-occurrence-violation "ty1 should not occur in ty2: ty1=~s, ty2=~s, exp=~s" ty1 ty2 exp)
  )
)

; Type * Type * Exp -> ()
(define report-unification-failure
  (lambda (ty1 ty2 exp)
    (eopl:error 'unification-failure "ty1=~s, ty2=~s, exp=~s" ty1 ty2 exp)
  )
)



; OptionalType -> Type
(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (fresh-tvar-type))
      (a-type (ty) ty)
    )
  )
)

; Serial Number = Int
(define the-sn 'uninitialized)

(define init-sn!
  (lambda () (set! the-sn 0))
)

; () -> Type
(define fresh-tvar-type
  (lambda ()
    (set! the-sn (+ the-sn 1))
    (tvar-type the-sn)
  )
)


; S-exp = Sym | Listof(S-exp)
; TvarTypeSym = a symbol ending with a digit
; A-list = Listof(Pair(TvarTypeSym, TvarTypeSym))

; Type * Type -> Bool
(define equal-types?
  (lambda (ty1 ty2)
    (equal-up-to-gensyms? ty1 ty2)
  )
)

; S-exp * S-exp -> Bool
(define equal-up-to-gensyms?
  (lambda (sexp1 sexp2)
    (equal?
      (apply-subst-to-sexp (canonical-subst sexp1) sexp1)
      (apply-subst-to-sexp (canonical-subst sexp2) sexp2)
    )
  )
)

; S-exp -> A-list
(define canonical-subst
  (lambda (sexp)
    ; S-exp * A-list -> A-list
    (let loop ([sexp sexp] [table '()])
      (cond
        ((null? sexp) table)
        ((tvar-type-sym? sexp)
          (cond
            ((assq sexp table) table)
            (else
              (cons
                (cons sexp (ctr->ty (length table)))
                table
              )
            )
          )
        )
        ((pair? sexp)
          (loop (cdr sexp) (loop (car sexp) table))
        )
        (else table)
      )
    )
  )
)

; Sym -> Bool
(define tvar-type-sym?
  (lambda (sym)
    (and (symbol? sym)
      (char-numeric? (car (reverse (symbol->list sym))))
    )
  )
)

; Sym -> List
(define symbol->list
  (lambda (x)
    (string->list (symbol->string x))
  )
)

; A-list * S-exp -> S-exp
(define apply-subst-to-sexp
  (lambda (subst sexp)
    (cond
      ((null? sexp) sexp)
      ((tvar-type-sym? sexp) (cdr (assq sexp subst)))
      ((pair? sexp)
        (cons
          (apply-subst-to-sexp subst (car sexp))
          (apply-subst-to-sexp subst (cdr sexp))
        )
      )
      (else sexp)
    )
  )
)

; N -> Sym
(define ctr->ty
  (lambda (n)
    (string->symbol
      (string-append "tvar" (number->string n))
    )
  )
)