module T : sig

type t = [
| `fcvt_l_s
| `fcvt_lu_s
| `fcvt_s_l
| `fcvt_s_lu
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm : sig

val fcvt_l_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_lu_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_s_l : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_s_lu : rd:int -> rs1:int -> rm:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

