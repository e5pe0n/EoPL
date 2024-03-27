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
    (program ((arbno module-definition) expression) a-program)

    (module-definition
      ("module" identifier "interface" interface "body" module-body)
      a-module-definition
    )
    (interface ("[" (arbno declaration) "]") simple-iface)
    (interface ("(" "(" identifier ":" interface ")" "=>" interface ")") proc-iface)
    (declaration (identifier ":" type) val-decl)
    (declaration ("opaque" identifier) opaque-type-decl)
    (declaration ("transparent" identifier "=" type) transparent-type-decl)
    (module-body ("[" (arbno definition) "]") defns-module-body)
    (module-body
      ("module-proc" "(" identifier ":" interface ")" module-body)
      proc-module-body
    )
    (module-body (identifier) var-module-body)
    (module-body ("(" identifier identifier ")") app-module-body)
    (definition (identifier "=" expression) val-defn)
    (definition ("type" identifier "=" type) type-defn)

    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("from" identifier "take" identifier) qualified-var-exp)
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
      ("let" identifier "=" expression "in" expression)
      let-exp
    )
    (expression
      ("letrec" type identifier "(" identifier ":" type ")" "=" expression "in" expression)
      letrec-exp
    )
    (expression
      ("proc" "(" identifier ":" type ")" expression)
      proc-exp
    )
    (expression ("(" expression expression ")") call-exp)

    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" type "->" type ")") proc-type)
    (type (identifier) named-type)
    (type ("from" identifier "take" identifier) qualified-type)
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


; Type -> Bool
(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) #t)
      (else #f)
    )
  )
)

; Type -> ()
(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) arg-type)
      (else (eopl:error 'proc-type->arg-type "Not a proc-type: ~s" ty))
    )
  )
)

; Type -> ()
(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) result-type)
      (else (eopl:error 'proc-type->result-type "Not a proc-type: ~s" ty))
    )
  )
)

; Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type)
        )
      )
      (named-type (name) name)
      (qualified-type (mod-name var-name)
        (list 'from mod-name 'take var-name)
      )
    )
  )
)


; Sym * Listof(Defn) -> Maybe(Defn)
(define maybe-lookup-module-in-list
  (lambda (search-name module-defns)
    (if (null? module-defns)
      #f
      (let ([name (module-definition->name (car module-defns))])
        (if (eqv? search-name name)
          (car module-defns)
          (maybe-lookup-module-in-list search-name (cdr module-defns))
        )
      )
    )
  )
)

; Sym * Listof(Defn) -> Defn
(define lookup-module-in-list
  (lambda (search-name module-defns)
    (let ([maybe-m-defn (maybe-lookup-module-in-list search-name module-defns)])
      (if maybe-m-defn
        maybe-m-defn
        (eopl:error 'lookup-module-in-list "unknown module ~s" search-name)
      )
    )
  )
)

; Defn -> Sym
(define module-definition->name
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-interface m-body) m-name)
    )
  )
)

; Defn -> Type
(define module-definition->interface
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-interface m-body) m-interface)
    )
  )
)

; Defn -> Type
(define module-definition->body
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-interface m-body) m-body)
    )
  )
)

(define val-decl?
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) #t)
      (else #f)
    )
  )
)

(define transparent-type-decl?
  (lambda (decl)
    (cases declaration decl
      (transparent-type-decl (name ty) #t)
      (else #f)
    )
  )
)

(define opaque-type-decl?
  (lambda (decl)
    (cases declaration decl
      (opaque-type-decl (name) #t)
      (else #f)
    )
  )
)

; Decl -> Sym
(define decl->name
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) name)
      (transparent-type-decl (name ty) name)
      (opaque-type-decl (name) name)
    )
  )
)

; Decl -> Type
(define decl->type
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) ty)
      (transparent-type-decl (name ty) ty)
      (else (eopl:error 'decl->type "Couldn't extract type from ~s" decl))
    )
  )
)
