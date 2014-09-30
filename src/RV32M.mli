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

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

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

