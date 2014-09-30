module T : sig

type t = [
| `fcvt_l_s
| `fcvt_lu_s
| `fcvt_s_l
| `fcvt_s_lu
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val fcvt_l_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_lu_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_s_l : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_s_lu : rd:int -> rs1:int -> rm:int -> Types.I.t

end

