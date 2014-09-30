module T : sig

type t = [
| `amoadd_d
| `amoxor_d
| `amoor_d
| `amoand_d
| `amomin_d
| `amomax_d
| `amominu_d
| `amomaxu_d
| `amoswap_d
| `lr_d
| `sc_d
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val amoadd_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoxor_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoor_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoand_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amomin_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amomax_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amominu_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amomaxu_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoswap_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val lr_d : rd:int -> rs1:int -> aqrl:int -> Types.I.t
val sc_d : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t

end

