(* Some quick check helpers *)

let tuple1 x = x 
let tuple2 = QCheck.Arbitrary.pair
let tuple3 = QCheck.Arbitrary.triple
let tuple4 = QCheck.Arbitrary.quad
let tuple5 a b c d e st = QCheck.Arbitrary.(a st, b st, c st, d st, e st)

module PP = struct
let tuple1 x = x 
let tuple2 = QCheck.PP.pair
let tuple3 = QCheck.PP.triple
let tuple4 = QCheck.PP.quad
let tuple5 a b c d e (v,w,x,y,z) = 
  Printf.sprintf "(%s, %s, %s, %s, %s)"
    (a v) (b w) (c x) (d y) (e z)
end
