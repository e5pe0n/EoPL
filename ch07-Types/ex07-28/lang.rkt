#lang eopl

(provide (all-defined-out))

(define the-lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
  )
)

(define the-grammar
  '(
    (program (expression) a-program)

    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp
    )
    (expression
      ("zero?" "(" expression ")")
      zero?-exp
    )
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp
    )
    (expression (identifier) var-exp)
    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp
    )
    (expression
      ("letrec" optional-type identifier "(" identifier ":" optional-type ")" "=" expression "in" expression)
      letrec-exp
    )
    (expression
      ("proc" "(" identifier ":" optional-type ")" expression)
      proc-exp
    )
    (expression ("(" expression expression ")") call-exp)
    (expression ("newpair" "(" expression "," expression ")") pair-exp)
    (expression
      ("unpair" identifier identifier "=" expression "in" expression)
      unpair-exp
    )
    (expression
      ("list" "(" expression (arbno "," expression) ")")
      list-exp
    )
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)

    (optional-type ("?") no-type)
    (optional-type (type) a-type)

    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" type "->" type ")") proc-type)
    (type ("pairof" type "*" type) pair-type)
    (type ("listof" type) list-type)
    (type ("%tvar-type" number) tvar-type)
  )
)

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes the-lexical-spec the-grammar)
  )
)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar)
)

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar)
)


; Type -> Bool
(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (sn) #t)
      (else #f)
    )
  )
)

; Type -> Bool
(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) #t)
      (else #f)
    )
  )
)

; Type -> Bool
(define pair-type?
  (lambda (ty)
    (cases type ty
      (pair-type (t1 t2) #t)
      (else #f)
    )
  )
)

; Type -> Bool
(define list-type?
  (lambda (ty)
    (cases type ty
      (list-type (t) #t)
      (else #f)
    )
  )
)

; Type -> Type
(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) arg-type)
      (else (report-invalid-type-error 'proc-type ty))
    )
  )
)

; Type -> Type
(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) result-type)
      (else (report-invalid-type-error 'proc-type ty))
    )
  )
)

; Type -> Type
(define pair-type->fst-type
  (lambda (ty)
    (cases type ty
      (pair-type (t1 t2) t1)
      (else (report-invalid-type-error 'pair-type ty))
    )
  )
)

; Type -> Type
(define pair-type->snd-type
  (lambda (ty)
    (cases type ty
      (pair-type (t1 t2) t2)
      (else (report-invalid-type-error 'pair-type ty))
    )
  )
)

; Type -> Type
(define list-type->t
  (lambda (ty)
    (cases type ty
      (list-type (t) t)
      (else (report-invalid-type-error 'list-type ty))
    )
  )
)

; Sym * Type -> ()
(define report-invalid-type-error
  (lambda (expected-ty actual-ty)
    (eopl:error 'invalid-type-error "type~= is expected; actual type=~s" expected-ty actual-ty)
  )
)

; Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type)
        )
      )
      (pair-type (t1 t2)
        (list 'pairof (type-to-external-form t1) (type-to-external-form t2))
      )
      (list-type (t)
        (list 'listof (type-to-external-form t))
      )
      (tvar-type (sn)
        (string->symbol
          (string-append "ty" (number->string sn))
        )
      )
    )
  )
)
