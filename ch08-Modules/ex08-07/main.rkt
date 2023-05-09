#lang eopl

(require "utils.rkt")
(require "lang.rkt")
(require "check-mods.rkt")
(require "interp.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (let ([pgm (scan&parse string)])
      (type-of-program pgm)
      (value-of-program pgm)
    )
  )
)

(println (run "
  module m1
    interface [
      a: int
      b: int
      c: int
    ]
    body [
      a = 33
      x = -(a, 1) % = 32
      b = -(a, x) % = 1
      c = -(x, b) % = 31
    ]
  let a = 10
  in -(-(from m1 take a, from m1 take b), a)
"))
; #(struct:num-val 22)

; (println (run "
;   module m1
;     interface [
;       u: bool
;     ]
;     body [
;       u = 33
;     ]
;   44
; "))
; ; (error-in-defn-of-module:
; ;  m1
; ;  expected-iface:
; ;  #(struct:simple-iface (#(struct:val-decl u #(struct:bool-type))))
; ;  actual-iface:
; ;  #(struct:simple-iface (#(struct:val-decl u #(struct:int-type)))))
; ; error: type-of-module-defn

; (println (run "
;   module m1
;     interface [
;       u: int
;       v: int
;     ]
;     body [
;       u = 33
;     ]
;   44
; "))
; ; (error-in-defn-of-module:
; ;  m1
; ;  expected-iface:
; ;  #(struct:simple-iface
; ;    (#(struct:val-decl u #(struct:int-type))
; ;     #(struct:val-decl v #(struct:int-type))))
; ;  actual-iface:
; ;  #(struct:simple-iface (#(struct:val-decl u #(struct:int-type)))))
; ; error: type-of-module-defn

; (println (run "
;   module m1
;     interface [
;       u: int
;       v: int
;     ]
;     body [
;       v = 33
;       u = 44
;     ]
;   from m1 take u
; "))
; ; (error-in-defn-of-module:
; ;  m1
; ;  expected-iface:
; ;  #(struct:simple-iface
; ;    (#(struct:val-decl u #(struct:int-type))
; ;     #(struct:val-decl v #(struct:int-type))))
; ;  actual-iface:
; ;  #(struct:simple-iface
; ;    (#(struct:val-decl v #(struct:int-type))
; ;     #(struct:val-decl u #(struct:int-type)))))
; ; error: type-of-module-defn

(println (run "
  module m1
    interface [
      u: int
    ]
    body [
      u = 44
    ]

  module m2
    interface [
      v: int
    ]
    body [
      v = -(from m1 take u, 11)
    ]

  -(from m1 take u, from m2 take v)
"))
; #(struct:num-val 11)

; (println (run "
;   module m2
;     interface [
;       v: int
;     ]
;     body [
;       v = -(from m1 take u, 11)
;     ]

;   module m1
;     interface [
;       u: int
;     ]
;     body [
;       u = 44
;     ]

;   -(from m1 take u, from m2 take v)
; "))
; ; (tenv-lookup-failure: (missing: module m1) in: #(struct:empty-tenv))

; (println (run "
;   module m
;     interface [
;       u: int
;     ]
;     body [
;       u = 11
;     ]

;   module m
;     interface [
;       u: int
;     ]
;     body [
;       u = 11
;     ]

;   100
; "))
; ; module-name-dupulicated: module named m is already defined

; (println (run "
;   module m
;     interface [
;       u: int
;       x: int
;     ]
;     body [
;       u = 11
;       v = 22
;       x = -(u, v)
;     ]

;   from m take v
; "))
; ; (lookup-variable-in-decls-failure:
; ;  (missing-variable v)
; ;  in:
; ;  (#(struct:val-decl u #(struct:int-type))
; ;   #(struct:val-decl x #(struct:int-type))))

(println (run "
  module m
    interface [
      u: int
      x: int
    ]
    body [
      u = 11
      v = 22
      x = -(u, v)
    ]

  from m take x
"))
; #(struct:num-val -11)

; (println (run "
;   module let-x
;     interface [
;       x: int
;     ]
;     body
;       let a = 11
;       in [
;         x = -(a, 1)
;       ]
;   from let-x take x
; "))
; ; #(struct:num-val 10)

; (println (run "
;   module even-odd
;     interface [
;       even: (int -> bool)
;       odd: (int -> bool)
;     ]
;     body
;       letrec
;         bool local-odd(x: int) =
;                 if zero?(x) then zero?(1) else (local-even -(x, 1))
;         bool local-even(x: int) =
;                 if zero?(x) then zero?(0) else (local-odd -(x, 1))
;       in [
;         even = local-even
;         odd = local-odd
;       ]
;   (from even-odd take odd 13)
; "))
; ; #(struct:bool-val #t)

(println (run "
  module m1
    interface [
      u: int
      v: int
    ]
    body
      module m2
        interface [
          v: int
        ]
        body [
          v = 33
        ]
      [
        u = 44
        v = -(from m2 take v, 1)
      ]
  from m1 take v
"))
; #(struct:num-val 32)

(println (run "
  module m1
    interface [
      u: int
      n: [v: int]
    ]
    body
      module m2
        interface [
          v: int
        ]
        body [
          v = 33
        ]
      [
        u = 44
        n = m2
      ]

  from m1 take n take v
"))
; #(struct:num-val 33)
