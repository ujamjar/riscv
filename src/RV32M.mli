module T : sig

type t = [
| `mul
| `mulh
| `mulhsu
| `mulhu
| `div
| `divu
| `rem
| `remu
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm : sig

val mul : rd:int -> rs1:int -> rs2:int -> Types.I.t
val mulh : rd:int -> rs1:int -> rs2:int -> Types.I.t
val mulhsu : rd:int -> rs1:int -> rs2:int -> Types.I.t
val mulhu : rd:int -> rs1:int -> rs2:int -> Types.I.t
val div : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divu : rd:int -> rs1:int -> rs2:int -> Types.I.t
val rem : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remu : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

