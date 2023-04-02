#lang eopl

(require "utils.rkt")
(require "type.rkt")
(require "lang.rkt")

(define type-check!
  (lambda (string)
    (type-of-program (scan&parse string))
  )
)

(println (type-check! "
  proc(x: int) -(x, 1)
"))
; #(struct:proc-type #(struct:int-type) #(struct:int-type))

(println (type-check! "
  letrec
    int double(x: int) = if zero?(x)
                          then 0
                          else -((double -(x, 1)), -2)
  in double
"))
; #(struct:proc-type #(struct:int-type) #(struct:int-type))

(println (type-check! "
  proc(f: (bool -> int)) proc(n: int) (f zero?(n))
"))
; #(struct:proc-type
;   #(struct:proc-type #(struct:bool-type) #(struct:int-type))
;   #(struct:proc-type #(struct:int-type) #(struct:int-type))
; )
