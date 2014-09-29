(* various instruction tests *)

module T64 = Types.Make(Types.D64)
module RV64 = Instr.Make(T64)
open T64

(* TODO...need asm dsl first to make this sensible *)

