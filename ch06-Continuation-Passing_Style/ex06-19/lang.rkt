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
    (program (inp-expression) a-program)

    (inp-expression (number) const-exp)
    (inp-expression
      ("-" "(" inp-expression "," inp-expression ")")
      diff-exp
    )
    (inp-expression
      ("*" "(" inp-expression "," inp-expression ")")
      mul-exp
    )
    (inp-expression
      ("zero?" "(" inp-expression ")")
      zero?-exp
    )
    (inp-expression
      ("if" inp-expression "then" inp-expression "else" inp-expression)
      if-exp
    )
    (inp-expression (identifier) var-exp)
    (inp-expression
      ("let" (arbno identifier "=" inp-expression) "in" inp-expression)
      let-exp
    )
    (inp-expression
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")" "=" inp-expression)
        "in" inp-expression
      )
      letrec-exp
    )
    (inp-expression
      ("proc" "(" (arbno identifier) ")" inp-expression)
      proc-exp
    )
    (inp-expression
      ("(" inp-expression (arbno inp-expression) ")")
      call-exp
    )

    (inp-expression
      ("emptylist")
      emptylist-exp
    )
    (inp-expression
      ("null?" "(" inp-expression ")")
      null?-exp
    )
    (inp-expression
      ("number?" "(" inp-expression ")")
      number?-exp
    )
    (inp-expression
      ("equal?" "(" inp-expression "," inp-expression ")")
      equal?-exp
    )
    (inp-expression
      ("less?" "(" inp-expression "," inp-expression ")")
      less?-exp
    )
    (inp-expression
      ("greater?" "(" inp-expression "," inp-expression ")")
      greater?-exp
    )
    (inp-expression
      ("list" "(" (separated-list inp-expression ",") ")")
      list-exp
    )
    (inp-expression
      ("cons" "(" inp-expression "," inp-expression ")")
      cons-exp
    )
    (inp-expression
      ("car" "(" inp-expression ")")
      car-exp
    )
    (inp-expression
      ("cdr" "(" inp-expression ")")
      cdr-exp
    )
    (inp-expression
      ("add1" "(" inp-expression ")")
      add1-exp
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
