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
          (init-sn!)
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
      (let-exp (vars exps body)
        (let (
            [p (reduce
                  (lambda (acc v)
                    (let ([tys (car acc)] [subst (cdr acc)])
                      (cases answer (type-of v tenv subst)
                        (an-answer (ty subst)
                          (cons (cons ty tys) subst)
                        )
                      )
                    )
                  )
                  exps
                  (cons '() subst)
                )
            ]
          )
          (let ([exp-tys (reverse (car p))] [subst (cdr p)])
            (type-of body (extend-tenv* vars exp-tys tenv) subst)
          )
        )
      )
      (proc-exp (vars otypes body)
        (let ([var-types (map (lambda (otype) (otype->type otype)) otypes)])
          (cases answer (type-of body (extend-tenv* vars var-types tenv) subst)
            (an-answer (body-type subst)
              (an-answer (proc-type var-types body-type) subst)
            )
          )
        )
      )
      (call-exp (rator rands)
        (let ([result-type (fresh-tvar-type)])
          (cases answer (type-of rator tenv subst)
            (an-answer (rator-type subst)
              (let* (
                  [p (reduce
                        (lambda (acc v)
                          (let ([rand-types (car acc)] [subst (cdr acc)])
                            (cases answer (type-of v tenv subst)
                              (an-answer (rand-type subst)
                                (cons (cons rand-type rand-types) subst)
                              )
                            )
                          )
                        )
                        rands (cons '() subst)
                      )
                  ]
                  [rand-types (reverse (car p))]
                  [subst (cdr p)]
                )
                (let ([subst (unifier rator-type (proc-type rand-types result-type) subst exp)])
                  (an-answer result-type subst)
                )
              )
            )
          )
        )
      )
      (letrec-exp (p-result-otypes p-names b-varss b-var-otypess p-bodies letrec-body)
        (let (
            [p-result-types (map otype->type p-result-otypes)]
            [b-var-typess (map (lambda (b-var-otypes) (map otype->type b-var-otypes)) b-var-otypess)]
          )
          (let (
              [tenv-for-letrec-body
                (extend-tenv* p-names
                  (map (lambda (b-var-types p-result-type)
                          (proc-type b-var-types p-result-type)
                        )
                    b-var-typess p-result-types
                  )
                  tenv
                )
              ]
            )
            (let (
                [subst (reduce
                          (lambda (acc v)
                            (let (
                                [subst acc]
                                [p-body (car v)]
                                [b-vars (cadr v)]
                                [b-var-types (caddr v)]
                                [p-result-type (cadddr v)]
                              )
                              (cases answer (type-of p-body (extend-tenv* b-vars b-var-types tenv-for-letrec-body) subst)
                                (an-answer (p-body-type subst)
                                  (unifier p-body-type p-result-type subst p-body)
                                )
                              )
                            )
                          )
                      (zip (list p-bodies b-varss b-var-typess p-result-types))
                      subst
                    )
                ]
              )
              (type-of letrec-body tenv-for-letrec-body subst)
            )
          )
        )
      )
      (pair-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (cases answer (type-of exp2 tenv subst)
              (an-answer (ty2 subst)
                (an-answer (pair-type ty1 ty2) subst)
              )
            )
          )
        )
      )
      (unpair-exp (var1 var2 exp1 body)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty subst)
            (let* (
                [tvar1 (fresh-tvar-type)]
                [tvar2 (fresh-tvar-type)]
                [subst (unifier ty (pair-type tvar1 tvar2) subst exp)]
              )
              (type-of body
                (extend-tenv var2 tvar2
                  (extend-tenv var1 tvar1 tenv)
                )
                subst
              )
            )
          )
        )
      )
      (list-exp (exp1 exps)
        (let* (
            [p (reduce
                  (lambda (acc v)
                    (let ([tys (car acc)] [subst (cdr acc)])
                      (cases answer (type-of v tenv subst)
                        (an-answer (ty subst)
                          (cons (cons ty tys) subst)
                        )
                      )
                    )
                  )
                  (cons exp1 exps)
                  (cons '() subst)
                )
            ]
            [tys (reverse (car p))]
            [subst (cdr p)]
          )
          (let* (
              [ty (car tys)]
              [subst (reduce (lambda (acc v) (unifier v ty acc exp)) tys subst)
              ]
            )
            (an-answer (list-type ty) subst)
          )
        )
      )
      (cons-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (cases answer (type-of exp2 tenv subst)
              (an-answer (ty2 subst)
                (let ([subst (unifier (list-type ty1) ty2 subst exp)])
                  (an-answer ty2 subst)
                )
              )
            )
          )
        )
      )
      (car-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let* (
                [tvar (fresh-tvar-type)]
                [subst (unifier ty1 (list-type tvar) subst exp)]
              )
              (an-answer tvar subst)
            )
          )
        )
      )
      (cdr-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let* (
                [tvar (fresh-tvar-type)]
                [subst (unifier ty1 (list-type tvar) subst exp)]
              )
              (an-answer ty1 subst)
            )
          )
        )
      )
      (null?-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let* (
                [tvar (fresh-tvar-type)]
                [subst (unifier ty1 (list-type tvar) subst exp)]
              )
              (an-answer (bool-type) subst)
            )
          )
        )
      )
      (emptylist-exp ()
        (an-answer (list-type (fresh-tvar-type)) subst)
      )
      (newref-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (an-answer (ref-type ty1) subst)
          )
        )
      )
      (deref-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let* (
                [tvar (fresh-tvar-type)]
                [subst (unifier ty1 (ref-type tvar) subst exp)]
              )
              (an-answer tvar subst)
            )
          )
        )
      )
      (setref-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let* (
                [tvar (fresh-tvar-type)]
                [subst (unifier ty1 (ref-type tvar) subst exp)]
              )
              (cases answer (type-of exp2 tenv subst)
                (an-answer (ty2 subst)
                  (let ([subst (unifier tvar ty2 subst exp)])
                    (an-answer (void-type) subst)
                  )
                )
              )
            )
          )
        )
      )
      (begin-exp (exp1 exps)
        (reduce
          (lambda (acc v)
            (type-of v tenv
              (cases answer acc
                (an-answer (_ subst) subst)
              )
            )
          )
          exps (type-of exp1 tenv subst)
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
  (extend-tenv*
    (saved-vars (list-of symbol?))
    (saved-tys (list-of type?))
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
      (extend-tenv* (saved-vars saved-tys saved-tenv)
        (let loop ([saved-vars saved-vars] [saved-tys saved-tys])
          (if (null? saved-vars)
            (apply-tenv saved-tenv search-var)
            (if (eqv? search-var (car saved-vars))
              (car saved-tys)
              (loop (cdr saved-vars) (cdr saved-tys))
            )
          )
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
      (proc-type (arg-types result-type)
        (proc-type
          (map (lambda (arg-type) (apply-one-subst arg-type tvar ty1)) arg-types)
          (apply-one-subst result-type tvar ty1)
        )
      )
      (pair-type (t1 t2)
        (pair-type
          (apply-one-subst t1 tvar ty1)
          (apply-one-subst t2 tvar ty1)
        )
      )
      (list-type (t)
        (list-type (apply-one-subst t tvar ty1))
      )
      (ref-type (t)
        (ref-type (apply-one-subst t tvar ty1))
      )
      (void-type () (void-type))
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
      (proc-type (arg-types result-type)
        (proc-type
          (map (lambda (arg-type) (apply-subst-to-type arg-type subst)) arg-types)
          (apply-subst-to-type result-type subst)
        )
      )
      (pair-type (t1 t2)
        (pair-type
          (apply-subst-to-type t1 subst)
          (apply-subst-to-type t2 subst)
        )
      )
      (list-type (t)
        (list-type (apply-subst-to-type t subst))
      )
      (ref-type (t)
        (ref-type (apply-subst-to-type t subst))
      )
      (void-type () (void-type))
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
              [subst (reduce
                        (lambda (acc v) (unifier (car v) (cadr v) acc exp))
                        (zip
                          (list
                            (proc-type->arg-types ty1)
                            (proc-type->arg-types ty2)
                          )
                        )
                        subst
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
        ((and (pair-type? ty1) (pair-type? ty2))
          (let* (
              [subst (unifier
                        (pair-type->fst-type ty1)
                        (pair-type->fst-type ty2)
                        subst exp
                      )
              ]
              [subst (unifier
                        (pair-type->snd-type ty1)
                        (pair-type->snd-type ty2)
                        subst exp
                      )
              ]
            )
            subst
          )
        )
        ((and (list-type? ty1) (list-type? ty2))
          (unifier (list-type->t ty1) (list-type->t ty2) subst exp)
        )
        ((and (ref-type? ty1) (ref-type? ty2))
          (unifier (ref-type->t ty1) (ref-type->t ty2) subst exp)
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
      (proc-type (arg-types result-type)
        (and
          (all (lambda (t) (no-occurrence? tvar t)) arg-types)
          (no-occurrence? tvar result-type)
        )
      )
      (pair-type (t1 t2)
        (and
          (no-occurrence? tvar t1)
          (no-occurrence? tvar t2)
        )
      )
      (list-type (t) (no-occurrence? tvar t))
      (ref-type (t) (no-occurrence? tvar t))
      (void-type () #t)
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

(define init-sn!
  (lambda () (set! sn 0))
)

; () -> Type
(define fresh-tvar-type
  (lambda ()
    (let ([ty (tvar-type sn)])
      (set! sn (+ sn 1))
      ty
    )
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
