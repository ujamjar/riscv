module T : sig

type t = [
| `_custom0
| `_custom0_rs1
| `_custom0_rs1_rs2
| `_custom0_rd
| `_custom0_rd_rs1
| `_custom0_rd_rs1_rs2
| `_custom1
| `_custom1_rs1
| `_custom1_rs1_rs2
| `_custom1_rd
| `_custom1_rd_rs1
| `_custom1_rd_rs1_rs2
| `_custom2
| `_custom2_rs1
| `_custom2_rs1_rs2
| `_custom2_rd
| `_custom2_rd_rs1
| `_custom2_rd_rs1_rs2
| `_custom3
| `_custom3_rs1
| `_custom3_rs1_rs2
| `_custom3_rd
| `_custom3_rd_rs1
| `_custom3_rd_rs1_rs2
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm : sig

val _custom0 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom0_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom0_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom0_rd : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom0_rd_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom0_rd_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom1_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom1_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom1_rd : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom1_rd_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom1_rd_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom2_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom2_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom2_rd : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom2_rd_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom2_rd_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom3 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom3_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom3_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom3_rd : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom3_rd_rs1 : rd:int -> rs1:int -> imm12:int -> Types.I.t
val _custom3_rd_rs1_rs2 : rd:int -> rs1:int -> imm12:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

