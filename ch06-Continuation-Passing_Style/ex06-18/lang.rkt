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

    (const-or-var-expression (number) const-exp)
    (const-or-var-expression (identifier) var-exp)

    (simple-expression
      (const-or-var-expression)
      const-or-var-exp->exp
    )
    (simple-expression
      ("-" "(" const-or-var-expression "," const-or-var-expression ")")
      cps-diff-exp
    )
    (simple-expression
      ("*" "(" const-or-var-expression "," const-or-var-expression ")")
      cps-mul-exp
    )
    (simple-expression
      ("add1" "(" const-or-var-expression ")")
      cps-add1-exp
    )
    (simple-expression
      ("zero?" "(" const-or-var-expression ")")
      cps-zero?-exp
    )
    (simple-expression
      ("number?" "(" const-or-var-expression ")")
      cps-number?-exp
    )
    (simple-expression
      ("equal?" "(" const-or-var-expression "," const-or-var-expression ")")
      cps-equal?-exp
    )
    (simple-expression
      ("less?" "(" const-or-var-expression "," const-or-var-expression ")")
      cps-less?-exp
    )
    (simple-expression
      ("greater?" "(" const-or-var-expression "," const-or-var-expression ")")
      cps-greater?-exp
    )
    (simple-expression
      ("null?" "(" const-or-var-expression ")")
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
      ("list" "(" (separated-list const-or-var-expression ",") ")")
      cps-list-exp
    )
    (simple-expression
      ("car" "(" const-or-var-expression ")")
      cps-car-exp
    )
    (simple-expression
      ("cdr" "(" const-or-var-expression ")")
      cps-cdr-exp
    )
    (simple-expression
      ("cons" "(" const-or-var-expression "," const-or-var-expression ")")
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
