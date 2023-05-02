#lang eopl

(require "utils.rkt")
(require "lang.rkt")

(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (saved-var symbol?)
    (saved-val s-val?)
    (saved-env environment?)
  )
  (extend-env-rec
    (p-name identifier?)
    (b-var identifier?)
    (body expression?)
    (env environment?)
  )
  (extend-env-with-module
    (m-name symbol?)
    (m-val typed-module?)
    (saved-env environment?)
  )
)


(define-datatype proc proc?
  (procedure
    (var identifier?)
    (body expression?)
    (saved-env environment?)
  )
)

(define-datatype expval expval?
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
  )
  (proc-val
    (proc1 proc?)
  )
)

; ExpVal -> Number
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else report-expval-extractor-error 'expval->num val)
    )
  )
)

; ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else report-expval-extractor-error 'expval->bool val)
    )
  )
)

; ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else report-expval-extractor-error 'expval->proc val)
    )
  )
)

; String * ExpVal -> ()
(define report-expval-extractor-error
  (lambda (err-name val)
    (eopl:error err-name "Couldn't extract inside value; val=~s" val)
  )
)

(define-datatype typed-module typed-module?
  (simple-module (bindings environment?))
)
