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
    (program (tf-expression) a-program)

    (simple-expression (number) const-exp)
    (simple-expression (identifier) var-exp)
    (simple-expression
      ("-" "(" simple-expression "," simple-expression ")")
      cps-diff-exp
    )
    (simple-expression
      ("*" "(" simple-expression "," simple-expression ")")
      cps-mul-exp
    )
    (simple-expression
      ("add1" "(" simple-expression ")")
      cps-add1-exp
    )
    (simple-expression
      ("zero?" "(" simple-expression ")")
      cps-zero?-exp
    )
    (simple-expression
      ("number?" "(" simple-expression ")")
      cps-number?-exp
    )
    (simple-expression
      ("equal?" "(" simple-expression "," simple-expression ")")
      cps-equal?-exp
    )
    (simple-expression
      ("less?" "(" simple-expression "," simple-expression ")")
      cps-less?-exp
    )
    (simple-expression
      ("greater?" "(" simple-expression "," simple-expression ")")
      cps-greater?-exp
    )
    (simple-expression
      ("null?" "(" simple-expression ")")
      cps-null?-exp
    )
    (simple-expression
      ("proc" "(" (arbno identifier) ")" tf-expression)
      cps-proc-exp
    )
    (simple-expression
      ("emptylist")
      cps-emptylist-exp
    )
    (simple-expression
      ("list" "(" (separated-list simple-expression ",") ")")
      cps-list-exp
    )
    (simple-expression
      ("car" "(" simple-expression ")")
      cps-car-exp
    )
    (simple-expression
      ("cdr" "(" simple-expression ")")
      cps-cdr-exp
    )
    (simple-expression
      ("cons" "(" simple-expression "," simple-expression ")")
      cps-cons-exp
    )

    (tf-expression
      (simple-expression)
      simple-exp->exp
    )
    (tf-expression
      ("let" (arbno identifier "=" simple-expression) "in" tf-expression)
      cps-let-exp
    )
    (tf-expression
      ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" tf-expression) "in" tf-expression)
      cps-letrec-exp
    )
    (tf-expression
      ("if" simple-expression "then" tf-expression "else" tf-expression)
      cps-if-exp
    )
    (tf-expression
      ("(" simple-expression (arbno simple-expression) ")")
      cps-call-exp
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
