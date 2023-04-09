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
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
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
      ("letrec"
        (arbno type identifier "(" (separated-list identifier ":" type ",") ")" "=" expression)
        "in" expression
      )
      letrec-exp
    )
    (expression
      ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
      proc-exp
    )
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("set" identifier "=" expression) assign-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
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
    (expression ("emptylist_" type) emptylist-exp)

    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" (separated-list type "*") "->" type ")") proc-type)
    (type ("pairof" type "*" type) pair-type)
    (type ("listof" type) list-type)
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