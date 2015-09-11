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
] deriving(Enum,Bounded,Show)

val name : string

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

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

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

