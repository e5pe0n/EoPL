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

(println (run "
  let a = 44
      b = 11
      c = 12
  in -(-(a, b), c)
"))
; End of Computation.
; #(struct:num-val 21)

(println (run "
  let p1 = proc (x) -(x, 1)
      p2 = proc (p) (p 10)
  in (p2 p1)
"))
; End of Computation.
; #(struct:num-val 9)

(println (run "
  let p1 = proc (x, y) -(x, y)
  in (p1 10 1)
"))
; End of Computation.
; #(struct:num-val 9)

(println (run "
  let p1 = proc (x, y, z) -(-(x, y), z)
  in (p1 10 1 2)
"))
; End of Computation.
; #(struct:num-val 7)

(println (run "
  let f = proc (x) proc (y)
            begin
              set x = -(x, -1);
              -(x, y)
            end
  in ((f 44) 33)
"))
; #(struct:num-val 12)

(println (run "
    let g =
      let count = 0
      in proc (dummy)
          begin
            set count = -(count, -1);
            count
          end
    in let a = (g 11)
      in let b = (g 11)
        in -(a, b)
"))
; #(struct:num-val -1)

(println (run "
    let p = proc (x)
              set x = 4
    in let a = 3
      in begin
        (p a);
        a
      end
"))
; #(struct:num-val 3); arguments are passed by call-by-value

