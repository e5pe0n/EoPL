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

(print
  (run "
    let f = proc (x) proc (y)
        begin
          set x = -(x, -1);
          -(x, y)
        end
    in ((f 44) 33)
  ")
) ; #(struct:num-val 12)
(print
  (run "
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
  ")
) ; #(struct:num-val -1)
(print
  (run "
    let x = 0
    in letrec
        even(dummy) =
          if zero? (x)
          then 1
          else begin
              set x = -(x, 1);
              (odd 888)
            end
        odd(dummy) =
          if zero? (x)
          then 0
          else begin
              set x = -(x, 1);
              (even 888)
            end
      in begin
          set x = 13;
          (odd -888)
        end
  ")
) ; #(struct:num-val 1)
(print
  (run "
    let p = proc (x)
              set x = 4
    in let a = 3
      in begin
        (p a);
        a
      end
  ")
) ; #(struct:num-val 3) because arguments are passed by call-by-value

(print
  (run "
    let a = 3
    in let b = 4
    in let swap = proc (x) proc (y)
                    let tmp = deref(x)
                    in begin
                      setref(x, deref(y));
                      setref(y, tmp)
                    end
    in begin
      ((swap ref a) ref b);
      -(a, b)
    end
  ")
) ; #(struct:num-val 1)

; explicit-ref
;   ExpVal = Int + Bool + Proc + Ref(ExpVal)
;   DenVal = ExpVal
;
; implicit-ref
;   ExpVal = Int + Bool + Proc
;   DenVal = Ref(ExpVal)
;
; implicit-ref + `ref`
;   ExpVal = Int + Bool + Proc + Ref
;   DenVal = Ref(ExpVal)
