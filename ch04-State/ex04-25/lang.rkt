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
    (program (statement) a-program)

    (statement (identifier "=" expression) assign-stmt)
    (statement ("print" expression) print-stmt)
    (statement ("{" (separated-list statement ";") "}") begin-stmt)
    (statement ("if" expression statement statement) if-stmt)
    (statement ("while" expression statement) while-stmt)
    (statement
      ("var" (separated-list identifier ",") "=" (separated-list expression ",") ";" statement)
      var-stmt
    )
    (statement ("read" identifier) read-stmt)
    (statement ("do" statement "while" expression) do-while-stmt)

    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp
    )
    (expression
      ("+" "(" expression "," expression ")")
      add-exp
    )
    (expression
      ("*" "(" expression "," expression ")")
      mul-exp
    )
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("not" "(" expression ")") not-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
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
