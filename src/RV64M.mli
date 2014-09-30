module T : sig

type t = [
| `mulw
| `divw
| `divuw
| `remw
| `remuw
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val mulw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divuw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remuw : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

