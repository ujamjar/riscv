module T : sig

type t = [
| `fcvt_l_d
| `fcvt_lu_d
| `fmv_x_d
| `fcvt_d_l
| `fcvt_d_lu
| `fmv_d_x
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val fcvt_l_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_lu_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmv_x_d : rd:int -> rs1:int -> Types.I.t
val fcvt_d_l : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_d_lu : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmv_d_x : rd:int -> rs1:int -> Types.I.t

end

