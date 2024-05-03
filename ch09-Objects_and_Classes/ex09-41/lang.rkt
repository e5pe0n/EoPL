#lang eopl

(require "utils.rkt")

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
    (program ((arbno class-decl) expression) a-program)

    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp
    )
    (expression
      ("+" "(" expression "," expression ")")
      sum-exp
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
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp
    )
    (expression
      ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
      proc-exp
    )
    (expression
      ("(" expression (arbno expression) ")")
      call-exp
    )
    (expression
      ("letrec"
        (arbno type identifier "(" (separated-list identifier ":" type ",") ")" "=" expression)
        "in" expression
      )
      letrec-exp
    )
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp
    )
    (expression
      ("set" identifier "=" expression)
      assign-exp
    )
    (expression
      ("list" "(" expression (arbno "," expression) ")")
      list-exp
    )
    (expression ("print" "(" expression ")") print-exp)
    (expression
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp
    )
    (expression ("self") self-exp)
    (expression
      ("send" expression identifier
        "(" (separated-list expression ",") ")"
      )
      method-call-exp
    )
    (expression
      ("fieldref" expression identifier)
      fieldref-exp
    )
    (expression
      ("fieldset" expression identifier "=" expression)
      fieldset-exp
    )
    (expression
      ("super" identifier "(" (separated-list expression ",") ")")
      super-call-exp
    )
    (expression
      ("cast" expression identifier)
      cast-exp
    )
    (expression
      ("instanceof" expression identifier)
      instanceof-exp
    )

    (class-decl
      ("class" identifier
        "extends" identifier
        (arbno "implements" identifier)
        (arbno "field" type identifier)
        (arbno method-decl)
      )
      a-class-decl
    )
    (method-decl
      ("method" type identifier
        "(" (separated-list identifier ":" type ",") ")"
        expression
      )
      a-method-decl
    )

    (class-decl
      ("interface" identifier (arbno abstract-method-decl))
      an-interface-decl
    )
    (abstract-method-decl
      ("method" type identifier "(" (separated-list identifier ":" type ",") ")")
      an-abstract-method-decl
    )

    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("void") void-type)
    (type
      ("(" (separated-list type "*") "->" type ")")
      proc-type
    )
    (type ("listof" type) list-type)
    (type (identifier) class-type)
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


(define type->class-name
  (lambda (ty)
    (cases type ty
      (class-type (name) name)
      (else (eopl:error 'type->class-name "Not a class type: ~s" ty))
    )
  )
)
(define class-type?
  (lambda (ty)
    (cases type ty
      (class-type (name) #t)
      (else #f)
    )
  )
)

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (void-type () 'void)
      (class-type (name) name)
      (list-type (ty) (list 'listof (type-to-external-form ty)))
      (proc-type (arg-types result-type)
        (append
          (formal-types-to-external-form arg-types)
          '(->)
          (list (type-to-external-form result-type))
        )
      )
    )
  )
)
(define formal-types-to-external-form
  (lambda (types)
    (if (null? types)
      '()
      (if (null? (cdr types))
        (list (type-to-external-form (car types)))
        (cons
          (type-to-external-form (car types))
          (cons '*
            (formal-types-to-external-form (cdr types))
          )
        )
      )
    )
  )
)
