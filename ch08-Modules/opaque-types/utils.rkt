#lang eopl

(provide (all-defined-out))

; Any -> Bool
(define s-val?
  (lambda (x)
    #t
  )
)

; SchemeVal -> Bool
(define identifier?
  (lambda (x)
    (symbol? x)
  )
)

; SchemeVal -> ()
(define println
  (lambda (x)
    (eopl:printf "~s~%" x)
  )
)

; Var -> ()
(define report-no-binding-found
  (lambda (err-name search-var)
    (eopl:error err-name ": No binding for ~s" search-var)
  )
)

(define fresh-module-name
  (let ((sn 0))
    (lambda (module-name)
      (set! sn (+ sn 1))
      (string->symbol
        (string-append
          (symbol->string module-name)
          "%"             ; this can't appear in an input identifier
          (number->string sn)
        )
      )
    )
  )
)
