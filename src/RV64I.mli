module T : sig

type t = [
| `lwu
| `ld
| `sd
| `slli
| `srli
| `srai
| `addiw
| `slliw
| `srliw
| `sraiw
| `addw
| `subw
| `sllw
| `srlw
| `sraw
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val lwu : rd:int -> rs1:int -> imm12:int -> Types.I.t
val ld : rd:int -> rs1:int -> imm12:int -> Types.I.t
val sd : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val slli : rd:int -> rs1:int -> shamt:int -> Types.I.t
val srli : rd:int -> rs1:int -> shamt:int -> Types.I.t
val srai : rd:int -> rs1:int -> shamt:int -> Types.I.t
val addiw : rd:int -> rs1:int -> imm12:int -> Types.I.t
val slliw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val srliw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val sraiw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val addw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val subw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sllw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val srlw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sraw : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

