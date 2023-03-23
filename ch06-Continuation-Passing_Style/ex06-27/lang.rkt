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

(define cps-in-grammar
  '(
    (program (expression) a-program)

    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp
    )
    (expression
      ("*" "(" expression "," expression ")")
      mul-exp
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
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
        "in" expression
      )
      letrec-exp
    )
    (expression
      ("proc" "(" (arbno identifier) ")" expression)
      proc-exp
    )
    (expression
      ("(" expression (arbno expression) ")")
      call-exp
    )

    (expression
      ("emptylist")
      emptylist-exp
    )
    (expression
      ("null?" "(" expression ")")
      null?-exp
    )
    (expression
      ("number?" "(" expression ")")
      number?-exp
    )
    (expression
      ("equal?" "(" expression "," expression ")")
      equal?-exp
    )
    (expression
      ("less?" "(" expression "," expression ")")
      less?-exp
    )
    (expression
      ("greater?" "(" expression "," expression ")")
      greater?-exp
    )
    (expression
      ("list" "(" (separated-list expression ",") ")")
      list-exp
    )
    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp
    )
    (expression
      ("car" "(" expression ")")
      car-exp
    )
    (expression
      ("cdr" "(" expression ")")
      cdr-exp
    )
    (expression
      ("add1" "(" expression ")")
      add1-exp
    )
    (expression
      ("sum" "(" (separated-list expression ",") ")")
      sum-exp
    )
  )
)

(define cps-out-grammar
  '(
    (cps-program (tf-expression) a-cps-program)

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

    (simple-expression (number) cps-const-exp)
    (simple-expression (identifier) cps-var-exp)
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
      ("sum" "(" (separated-list simple-expression ",") ")")
      cps-sum-exp
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
  )
)

(sllgen:make-define-datatypes the-lexical-spec cps-in-grammar)
(sllgen:make-define-datatypes the-lexical-spec cps-out-grammar)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes the-lexical-spec cps-in-grammar)
  )
)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec cps-in-grammar)
)

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec cps-in-grammar)
)
