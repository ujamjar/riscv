module T : sig

type t = [
| `amoadd_w
| `amoxor_w
| `amoor_w
| `amoand_w
| `amomin_w
| `amomax_w
| `amominu_w
| `amomaxu_w
| `amoswap_w
| `lr_w
| `sc_w
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm : sig

val amoadd_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoxor_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoor_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoand_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amomin_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amomax_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amominu_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amomaxu_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val amoswap_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t
val lr_w : rd:int -> rs1:int -> aqrl:int -> Types.I.t
val sc_w : rd:int -> rs1:int -> rs2:int -> aqrl:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

