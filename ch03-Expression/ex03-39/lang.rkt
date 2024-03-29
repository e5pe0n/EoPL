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
    (expression
      ("cond" (arbno expression "==>" expression) "end")
      cond-exp
    )
    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp
    )
    (expression
      ("unpack" (arbno identifier) "=" expression "in" expression)
      unpack-exp
    )
    (expression (identifier) var-exp)
    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp
    )
    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp
    )
    (expression ("emptylist") emptylist-exp)
    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp
    )
    (expression ("(" expression expression ")") call-exp)
    (expression ("%lexref" number) nameless-var-exp)
    (expression ("%let" expression "in" expression) nameless-let-exp)
    (expression ("%lexproc" expression) nameless-proc-exp)
    (expression ("%unpack" expression "in" expression) nameless-unpack-exp)
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
