#lang eopl

(require "data-structures.rkt")
(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

; Answer = Type * Subst

(define-datatype answer answer?
  (an-answer
    (ty type?)
    (subst substitution?)
  )
)

; Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (begin
          (init-sn)
          (cases answer (type-of exp1 (init-tenv) (empty-subst))
            (an-answer (ty subst)
              (apply-subst-to-type ty subst)
            )
          )
        )
      )
    )
  )
)

; Exp * Tenv * Subst -> Type
(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      (const-exp (num) (an-answer (int-type) subst))
      (var-exp (var) (an-answer (apply-tenv tenv var) subst))
      (diff-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst1)
            (let ([subst1 (unifier ty1 (int-type) subst1 exp1)])
              (cases answer (type-of exp2 tenv subst1)
                (an-answer (ty2 subst2)
                  (let ([subst2 (unifier ty2 (int-type) subst2 exp2)])
                    (an-answer (int-type) subst2)
                  )
                )
              )
            )
          )
        )
      )
      (zero?-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst1)
            (let ([subst2 (unifier ty1 (int-type) subst1 exp)])
              (an-answer (bool-type) subst2)
            )
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let ([subst (unifier ty1 (bool-type) subst exp1)])
              (cases answer (type-of exp2 tenv subst)
                (an-answer (ty2 subst)
                  (cases answer (type-of exp3 tenv subst)
                    (an-answer (ty3 subst)
                      (let ([subst (unifier ty2 ty3 subst exp)])
                        (an-answer ty2 subst)
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
        (cases answer (type-of exp1 tenv subst)
          (an-answer (exp1-type subst)
            (type-of body (extend-tenv var exp1-type tenv) subst)
          )
        )
      )
      (proc-exp (var otype body)
        (let ([var-type (otype->type otype)])
          (cases answer (type-of body (extend-tenv var var-type tenv) subst)
            (an-answer (body-type subst)
              (an-answer (proc-type var-type body-type) subst)
            )
          )
        )
      )
      (call-exp (rator rand)
        (let ([result-type (fresh-tvar-type)])
          (cases answer (type-of rator tenv subst)
            (an-answer (rator-type subst)
              (cases answer (type-of rand tenv subst)
                (an-answer (rand-type subst)
                  (let ([subst (unifier rator-type (proc-type rand-type result-type) subst exp)])
                    (an-answer result-type subst)
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
            (cases answer (type-of p-body (extend-tenv b-var b-var-type tenv-for-letrec-body) subst)
              (an-answer (p-body-type subst)
                (let ([subst (unifier p-body-type p-result-type subst p-body)])
                  (type-of letrec-body tenv-for-letrec-body subst)
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
  (lambda (ty subst)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (t1 t2)
        (proc-type
          (apply-subst-to-type t1 subst)
          (apply-subst-to-type t2 subst)
        )
      )
      (tvar-type (sn)
        (let ([tmp (assoc ty subst)])
          (if tmp (cdr tmp) ty)
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
  (list-of (pair-of tvar-type? type?))
)

; () -> Subst
(define empty-subst
  (lambda () '())
)

; Subst * Tvar * Type -> Subst
(define extend-subst
  (lambda (subst tvar ty)
    (cons
      (cons tvar ty)
      (map
        (lambda (p)
          (let ([oldlhs (car p)] [oldrhs (cdr p)])
            (cons oldlhs (apply-one-subst oldrhs tvar ty))
          )
        )
        subst
      )
    )
  )
)


; Type * Type * Subst * Exp -> Subst
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let (
        [ty1 (apply-subst-to-type ty1 subst)]
        [ty2 (apply-subst-to-type ty2 subst)]
      )
      (cond
        ((equal? ty1 ty2) subst)
        ((tvar-type? ty1)
          (if (no-occurrence? ty1 ty2)
            (extend-subst subst ty1 ty2)
            (report-no-occurrence-violation ty1 ty2 exp)
          )
        )
        ((tvar-type? ty2)
          (if (no-occurrence? ty2 ty1)
            (extend-subst subst ty2 ty1)
            (report-no-occurrence-violation ty2 ty1 exp)
          )
        )
        ((and (proc-type? ty1) (proc-type? ty2))
          (let* (
              [subst (unifier
                        (proc-type->arg-type ty1)
                        (proc-type->arg-type ty2)
                        subst exp
                      )
              ]
              [subst (unifier
                        (proc-type->result-type ty1)
                        (proc-type->result-type ty2)
                        subst exp
                      )
              ]
            )
            subst
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
(define sn 'uninitialized)

(define init-sn
  (lambda () (set! sn 0))
)

; () -> Type
(define fresh-tvar-type
  (lambda ()
    (set! sn (+ sn 1))
    (tvar-type sn)
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
