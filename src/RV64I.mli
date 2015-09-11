module T : sig

type t = [
| `lwu
| `ld
| `sd
| `slli64
| `srli64
| `srai64
| `slliw
| `srliw
| `sraiw
| `addiw
| `addw
| `subw
| `sllw
| `srlw
| `sraw
] deriving(Enum,Bounded,Show)

val name : string

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val lwu : rd:int -> rs1:int -> imm12:int -> Types.I.t
val ld : rd:int -> rs1:int -> imm12:int -> Types.I.t
val sd : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val slli64 : rd:int -> rs1:int -> shamt:int -> Types.I.t
val srli64 : rd:int -> rs1:int -> shamt:int -> Types.I.t
val srai64 : rd:int -> rs1:int -> shamt:int -> Types.I.t
val slliw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val srliw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val sraiw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val addiw : rd:int -> rs1:int -> imm12:int -> Types.I.t
val addw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val subw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sllw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val srlw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sraw : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

module Asm : sig

val lwu : rd:int -> rs1:int -> imm:int -> Types.I.t
val ld : rd:int -> rs1:int -> imm:int -> Types.I.t
val sd : rs1:int -> rs2:int -> imm:int -> Types.I.t
val slli64 : rd:int -> rs1:int -> imm:int -> Types.I.t
val srli64 : rd:int -> rs1:int -> imm:int -> Types.I.t
val srai64 : rd:int -> rs1:int -> imm:int -> Types.I.t
val slliw : rd:int -> rs1:int -> imm:int -> Types.I.t
val srliw : rd:int -> rs1:int -> imm:int -> Types.I.t
val sraiw : rd:int -> rs1:int -> imm:int -> Types.I.t
val addiw : rd:int -> rs1:int -> imm:int -> Types.I.t
val addw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val subw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sllw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val srlw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sraw : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

