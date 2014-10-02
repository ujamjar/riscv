module T : sig

type t = [
| `fcvt_l_d
| `fcvt_lu_d
| `fmv_x_d
| `fcvt_d_l
| `fcvt_d_lu
| `fmv_d_x
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm : sig

val fcvt_l_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_lu_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmv_x_d : rd:int -> rs1:int -> Types.I.t
val fcvt_d_l : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_d_lu : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmv_d_x : rd:int -> rs1:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

