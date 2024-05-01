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
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp
    )
    (expression
      ("(" expression (arbno expression) ")")
      call-exp
    )
    (expression
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
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
      ("list" "(" (separated-list expression ",") ")")
      list-exp
    )
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
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
      ("named-send" identifier expression identifier
        "(" (separated-list expression ",") ")"
      )
      named-method-call-exp
    )
    (expression
      ("super" identifier "(" (separated-list expression ",") ")")
      super-call-exp
    )
    (expression
      ("instanceof" expression identifier)
      instanceof-exp
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
      ("named-fieldref" identifier expression identifier)
      named-fieldref-exp
    )
    (expression
      ("named-fieldset" identifier expression identifier "=" expression)
      named-fieldset-exp
    )

    (class-decl
      ("class" identifier
        "extends" identifier
        (arbno "field" identifier identifier)
        (arbno method-decl)
      )
      a-class-decl
    )
    (method-decl
      ("method" identifier identifier
        "(" (separated-list identifier ",") ")"
        expression
      )
      a-method-decl
    )
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
