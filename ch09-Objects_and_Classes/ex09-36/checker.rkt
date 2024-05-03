#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "static-classes.rkt")
(require "static-data-structs.rkt")

(provide (all-defined-out))


(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls exp1)
        (initialize-static-class-env! class-decls)
        (for-each check-class-decl! class-decls)
        (type-of exp1 (init-tenv))
      )
    )
  )
)

(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      (var-exp (var) (apply-tenv tenv var))
      (diff-exp (exp1 exp2)
        (let (
            [type1 (type-of exp1 tenv)]
            [type2 (type-of exp2 tenv)]
          )
          (check-equal-type! type1 (int-type) exp1)
          (check-equal-type! type2 (int-type) exp2)
          (int-type)
        )
      )
      (sum-exp (exp1 exp2)
        (let (
            [type1 (type-of exp1 tenv)]
            [type2 (type-of exp2 tenv)]
          )
          (check-equal-type! type1 (int-type) exp1)
          (check-equal-type! type2 (int-type) exp2)
          (int-type)
        )
      )
      (zero?-exp (exp1)
        (let ([type1 (type-of exp1 tenv)])
          (check-equal-type! type1 (int-type) exp1)
          (bool-type)
        )
      )
      (if-exp (test-exp true-exp false-exp)
        (let (
            [test-type (type-of test-exp tenv)]
            [true-type (type-of true-exp tenv)]
            [false-type (type-of false-exp tenv)]
          )
          (check-equal-type! test-type (bool-type) test-exp)
          (check-equal-type! true-type false-type exp)
          true-type
        )
      )
      (let-exp (vars rands body)
        (let ([new-tenv (extend-tenv vars (types-of-exps rands tenv) tenv)])
          (type-of body new-tenv)
        )
      )
      (proc-exp (b-vars b-var-types body)
        (let ([result-type (type-of body (extend-tenv b-vars b-var-types tenv))])
          (proc-type b-var-types result-type)
        )
      )
      (call-exp (rator rands)
        (let (
            [rator-type (type-of rator tenv)]
            [rand-types (types-of-exps rands tenv)]
          )
          (type-of-call rator-type rand-types rands exp)
        )
      )
      (letrec-exp (p-result-types p-names b-varss b-var-typess p-bodies letrec-body)
        (let (
            [
              tenv-for-letrec-body
              (extend-tenv p-names
                (map proc-type b-var-typess p-result-types)
                tenv
              )
            ]
          )
          (for-each
            (lambda (p-result-type b-vars b-var-types p-body)
              (let (
                  [
                    p-body-type
                    (type-of p-body
                      (extend-tenv b-varss b-var-types tenv-for-letrec-body)
                    )
                  ]
                )
                (check-equal-type! p-body-type p-result-type p-body)
              )
            )
          )
          (type-of letrec-body tenv-for-letrec-body)
        )
      )
      (begin-exp (exp1 exps)
        (letrec (
            [
              type-of-begins
              (lambda (e1 es)
                (let ([t1 (type-of e1 tenv)])
                  (if (null? es)
                    t1
                    (type-of-begins (car es) (cdr es))
                  )
                )
              )
            ]
          )
          (type-of-begins exp1 exps)
        )
      )
      (assign-exp (var rhs)
        (check-is-subtype! (type-of rhs tenv) (apply-tenv tenv var) exp)
        (void-type)
      )
      (list-exp (exp1 exps)
        (let ([type-of-car (type-of exp1 tenv)])
          (for-each
            (lambda (e)
              (check-equal-type! (type-of e tenv) type-of-car e)
            )
            exps
          )
          (list-type type-of-car)
        )
      )
      (new-object-exp (class-name rands)
        (let (
            [arg-types (types-of-exps rands tenv)]
            [c (lookup-static-class class-name)]
          )
          (cases static-class c
            (an-interface (parent-i-names method-tenv)
              (report-cant-instantiate-interface class-name)
            )
            (a-static-class (super-name i-names f-names f-types method-tenv)
              (type-of-call (find-method-type class-name 'initialize) arg-types rands exp)
              (class-type class-name)
            )
          )
        )
      )
      (self-exp ()
        (apply-tenv tenv '%self)
      )
      (method-call-exp (obj-exp method-name rands)
        (let (
            [arg-types (types-of-exps rands tenv)]
            [obj-type (type-of obj-exp tenv)]
          )
          (if (eqv? method-name 'initialize)
            (eopl:error 'invalid-initialize-method-call "initialize method call not allowed")
            (type-of-call (find-method-type (type->class-name obj-type) method-name)
              arg-types rands exp
            )
          )
        )
      )
      (super-call-exp (method-name rands)
        (let (
            [arg-types (types-of-exps rands tenv)]
            [obj-type (apply-tenv tenv '%self)]
          )
          (type-of-call (find-method-type (apply-tenv tenv '%super) method-name)
            arg-types rands exp
          )
        )
      )
      (cast-exp (exp1 class-name)
        (let ([obj-type (type-of exp1 tenv)])
          (if (class-type? obj-type)
            (if (is-static-concrete-class? class-name)
              (if
                (or
                  (statically-is-subclass? class-name (type->class-name obj-type))
                  (and
                    (is-static-concrete-class? (type->class-name obj-type))
                    (statically-is-subclass? (type->class-name obj-type) class-name)
                  )
                )
                (class-type class-name)
                (report-bad-cast-to-incompatible-type obj-type class-name exp)
              )
              (report-bad-cast-with-iface class-name exp)
            )
            (report-bad-type-to-cast obj-type exp1)
          )
        )
      )
      (instanceof-exp (exp1 class-name)
        (let ([obj-type (type-of exp1 tenv)])
          (if (class-type? obj-type)
            (if (is-static-concrete-class? class-name)
              (if
                (or
                  (statically-is-subclass? class-name (type->class-name obj-type))
                  (and
                    (is-static-concrete-class? (type->class-name obj-type))
                    (statically-is-subclass? (type->class-name obj-type) class-name)
                  )
                )
                (bool-type)
                (report-bad-instanceof-to-incompatible-type obj-type class-name exp)
              )
              (report-bad-instanceof-with-iface class-name exp)
            )
            (report-bad-type-to-instanceof obj-type exp1)
          )
        )
      )
    )
  )
)

(define report-cant-instantiate-interface
  (lambda (class-name)
    (eopl:error 'type-of-new-obj-exp "can't instantiate interface ~s" class-name)
  )
)

(define types-of-exps
  (lambda (rands tenv)
    (map (lambda (exp) (type-of exp tenv)) rands)
  )
)

(define type-of-call
  (lambda (rator-type rand-types rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
        (when (not (= (length arg-types) (length rand-types)))
          (report-wrong-number-of-arguments arg-types rand-types exp)
        )
        (for-each check-is-subtype! rand-types arg-types rands)
        result-type
      )
      (else
        (report-rator-not-of-proc-type (type-to-external-form rator-type) exp)
      )
    )
  )
)

(define report-rator-not-of-proc-type
  (lambda (external-form-rator-type exp)
    (eopl:error 'type-of-call "rator ~s is not of proc-type ~s"
      exp external-form-rator-type
    )
  )
)
(define report-wrong-number-of-arguments
  (lambda (arg-types rand-types exp)
    (eopl:error 'type-of-call
      "these are not the same: ~s and ~s in ~s"
      (map type-to-external-form arg-types)
      (map type-to-external-form rand-types)
      exp
    )
  )
)

(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name parent-i-names abs-method-decls)
        (for-each
          (lambda (parent-i-name)
            (check-if-extends! i-name parent-i-name)
          )
          parent-i-names
        )
      )
      (a-class-decl (class-name super-name i-names f-types f-names method-decls)
        (let ([sc (lookup-static-class class-name)])
          (for-each
            (lambda (method-decl)
              (check-method-decl! method-decl class-name super-name
                (static-class->field-names sc) (static-class->field-types sc)
              )
            )
            method-decls
          )
        )
        (for-each
          (lambda (i-name)
            (check-if-implements! class-name i-name)
          )
          i-names
        )
      )
    )
  )
)
(define check-method-decl!
  (lambda (m-decl self-name super-name f-names f-types)
    (cases method-decl m-decl
      (a-method-decl (res-type m-name vars var-types body)
        (let* (
            [
              tenv
              (extend-tenv vars var-types
                (extend-tenv-with-self-and-super
                  (class-type self-name) super-name
                  (extend-tenv f-names f-types
                    (init-tenv)
                  )
                )
              )
            ]
            [body-type (type-of body tenv)]
          )
          (check-is-subtype! body-type res-type m-decl)
          (if (eqv? m-name 'initialize)
            #t
            (let (
                [
                  maybe-super-type
                  (maybe-find-method-type
                    (static-class->method-tenv
                      (lookup-static-class super-name)
                    )
                    m-name
                  )
                ]
              )
              (if maybe-super-type
                (check-is-subtype!
                  (proc-type var-types res-type) maybe-super-type body
                )
                #t
              )
            )
          )
        )
      )
    )
  )
)
(define check-if-implements!
  (lambda (c-name i-name)
    (cases static-class (lookup-static-class i-name)
      (a-static-class (s-name i-names f-names f-types m-tenv)
        (report-cant-implement-non-interface c-name i-name)
      )
      (an-interface (parent-i-names method-tenv)
        (let ([class-method-tenv (static-class->method-tenv (lookup-static-class c-name))])
          (for-each
            (lambda (method-binding)
              (let (
                  [m-name (car method-binding)]
                  [m-type (cadr method-binding)]
                )
                (let ([c-method-type (maybe-find-method-type class-method-tenv m-name)])
                  (if c-method-type
                    (check-is-subtype! c-method-type m-type c-name)
                    (report-implements-missing-method c-name i-name m-name)
                  )
                )
              )
            )
            method-tenv
          )
        )
      )
    )
  )
)
(define check-if-extends!
  (lambda (i-name parent-i-name)
    (cases static-class (lookup-static-class parent-i-name)
      (a-static-class (s-name i-names f-names f-types m-tenv)
        (report-cant-implement-non-interface i-name parent-i-name)
      )
      (an-interface (parent-i-names method-tenv)
        (let ([class-method-tenv (static-class->method-tenv (lookup-static-class i-name))])
          (for-each
            (lambda (method-binding)
              (let (
                  [m-name (car method-binding)]
                  [m-type (cadr method-binding)]
                )
                (let ([i-method-type (maybe-find-method-type class-method-tenv m-name)])
                  (if i-method-type
                    (check-is-subtype! i-method-type m-type i-name)
                    (report-extends-missing-method i-name parent-i-name m-name)
                  )
                )
              )
            )
            method-tenv
          )
        )
      )
    )
  )
)

(define report-cant-implement-non-interface
  (lambda (c-name i-name)
    (eopl:error 'check-if-implements
      "class ~s claims to implement non-interface ~s"
      c-name i-name
    )
  )
)
(define report-implements-missing-method
  (lambda (c-name i-name i-m-name)
    (eopl:error 'check-if-implements
      "class ~s claims to implement ~s, missing method ~s"
      c-name i-name i-m-name
    )
  )
)
(define report-extends-missing-method
  (lambda (i-name parent-i-name i-m-name)
    (eopl:error 'check-if-extends
      "interface ~s claims to define all methods in interface ~s, missing method ~s"
      parent-i-name i-name i-m-name
    )
  )
)

(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (equal? t1 t2)
      #t
      (eopl:error 'type-of
        "types didn't match: ~s ~= ~s in~%~s"
        (type-to-external-form t1)
        (type-to-external-form t2)
        exp
      )
    )
  )
)
(define check-is-subtype!
  (lambda (t1 t2 exp)
    (if (is-subtype? t1 t2)
      #t
      (report-subtype-failure
        (type-to-external-form t1)
        (type-to-external-form t2)
        exp
      )
    )
  )
)
(define report-subtype-failure
  (lambda (external-form-t1 external-form-t2 exp)
    (eopl:error 'check-is-subtype!
      "~s is not a subtype of ~s in ~%~s"
      external-form-t1
      external-form-t2
      exp
    )
  )
)

(define is-subtype?
  (lambda (t1 t2)
    (cases type t1
      (class-type (name1)
        (cases type t2
          (class-type (name2)
            (statically-is-subclass? name1 name2)
          )
          (else #f)
        )
      )
      (proc-type (args1 res1)
        (cases type t2
          (proc-type (args2 res2)
            (and
              (every2? is-subtype? args2 args1)
              (is-subtype? res1 res2)
            )
          )
          (else #f)
        )
      )
      (else (equal? t1 t2))
    )
  )
)
(define andmap
  (lambda (pred xs ys)
    (cond
      ((and (null? xs) (null? ys)) #t)
      ((or (null? xs) (null? ys)) #f)
      ((pred (car xs) (car ys))
        (andmap pred (cdr xs) (cdr ys))
      )
      (else #f)
    )
  )
)
(define every2? andmap)

(define statically-is-subclass?
  (lambda (name1 name2)
    (or
      (eqv? name1 name2)
      (let ([super-name (static-class->super-name (lookup-static-class name1))])
        (if (eqv? super-name 'object)
          #f
          (statically-is-subclass? super-name name2)
        )
      )
      (let ([i-names (static-class->interface-names (lookup-static-class name1))])
        (memv name2 i-names)
      )
    )
  )
)

(define report-bad-type-to-cast
  (lambda (ty exp)
    (eopl:error 'bad-type-to-cast
      "can't cast non-object; ~s had type ~s"
      exp
      (type-to-external-form ty)
    )
  )
)
(define report-bad-cast-with-iface
  (lambda (i-name exp)
    (eopl:error 'bad-cast-with-interface
      "can't cast to interface ~s in ~s; interface can't have object"
      i-name
      exp
    )
  )
)
(define report-bad-cast-to-incompatible-type
  (lambda (ty class-name exp)
    (eopl:error 'bad-cast-to-incompatible-type
      "can't cast type ~s to type ~s~%in ~s"
      (type-to-external-form ty)
      class-name
      exp
    )
  )
)

(define report-bad-type-to-instanceof
  (lambda (ty exp)
    (eopl:error 'bad-type-to-instanceof
      "can't apply instanceof to non-object; ~s had type ~s"
      exp
      (type-to-external-form ty)
    )
  )
)
(define report-bad-instanceof-with-iface
  (lambda (i-name exp)
    (eopl:error 'bad-instanceof-with-interface
      "can't apply instanceof with interface ~s in ~s; interface can't have object"
      i-name
      exp
    )
  )
)
(define report-bad-instanceof-to-incompatible-type
  (lambda (ty class-name exp)
    (eopl:error 'bad-instanceof-to-incompatible-type
      "this instanceof never returns true; type ~s is incompatible with type ~s:~%in ~s"
      (type-to-external-form ty)
      class-name
      exp
    )
  )
)
