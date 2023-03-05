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
      ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression)
      letrec-exp
    )
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp
    )
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression
      ("cons" "(" expression "," expression")")
      cons-exp
    )
    (expression ("emptylist") emptylist-exp)
    (expression
      ("null?" "(" expression ")")
      null?-exp
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
      ("list" "(" (separated-list expression ",") ")")
      list-exp
    )
    (expression ("set" identifier "=" expression) assign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp
    )
    (expression ("print" "(" expression ")") print-exp)
    (expression ("spawn" "(" expression ")") spawn-exp)
    (expression ("mutex" "(" ")") mutex-exp)
    (expression ("wait" "(" expression ")") wait-exp)
    (expression ("signal" "(" expression ")") signal-exp)
    (expression ("yield") yield-exp)
    (expression ("kill" "(" expression ")") kill-exp)
    (expression ("send" "(" expression "," expression ")") send-exp)
    (expression ("recv" "(" ")") recv-exp)
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
