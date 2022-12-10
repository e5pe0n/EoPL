#lang eopl

(require "lang.rkt")
(require "interp.rkt")
(require "utils.rkt")

; String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))
  )
)

; (println (run "
;   let a = 44
;       b = 11
;       c = 12
;   in -(-(a, b), c)
; "))
; ; End of Computation.
; ; #(struct:num-val 21)

; (println (run "
;   let p1 = proc (x) -(x, 1)
;       p2 = proc (p) (p 10)
;   in (p2 p1)
; "))
; ; End of Computation.
; ; #(struct:num-val 9)

; (println (run "
;   let p1 = proc (x, y) -(x, y)
;   in (p1 10 1)
; "))
; End of Computation.
; #(struct:num-val 9)

; (println (run "
;   let p1 = proc (x, y, z) -(-(x, y), z)
;   in (p1 10 1 2)
; "))
; End of Computation.
; #(struct:num-val 7)

(println (run "
  let x = 101
      y = 0
  in try / (x, y)
      catch (x) x
"))
; End of Computation.
; #(struct:num-val 1)
