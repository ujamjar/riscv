module T : sig

type t = [
| `beq
| `bne
| `blt
| `bge
| `bltu
| `bgeu
| `jalr
| `jal
| `lui
| `auipc
| `addi
| `slti
| `sltiu
| `xori
| `ori
| `andi
| `add
| `sub
| `sll
| `slt
| `sltu
| `xor_
| `srl
| `sra
| `or_
| `and_
| `slliw
| `srliw
| `sraiw
| `lb
| `lh
| `lw
| `lbu
| `lhu
| `sb
| `sh
| `sw
| `fence
| `fence_i
| `scall
| `sbreak
| `_rdcycle
| `_rdtime
| `_rdinstret
| `_rdcycleh
| `_rdtimeh
| `_rdinstreth
] deriving(Enum,Bounded,Show)

val name : string

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val beq : bimm12hi:int -> rs1:int -> rs2:int -> bimm12lo:int -> Types.I.t
val bne : bimm12hi:int -> rs1:int -> rs2:int -> bimm12lo:int -> Types.I.t
val blt : bimm12hi:int -> rs1:int -> rs2:int -> bimm12lo:int -> Types.I.t
val bge : bimm12hi:int -> rs1:int -> rs2:int -> bimm12lo:int -> Types.I.t
val bltu : bimm12hi:int -> rs1:int -> rs2:int -> bimm12lo:int -> Types.I.t
val bgeu : bimm12hi:int -> rs1:int -> rs2:int -> bimm12lo:int -> Types.I.t
val jalr : rd:int -> rs1:int -> imm12:int -> Types.I.t
val jal : rd:int -> jimm20:int -> Types.I.t
val lui : rd:int -> imm20:int -> Types.I.t
val auipc : rd:int -> imm20:int -> Types.I.t
val addi : rd:int -> rs1:int -> imm12:int -> Types.I.t
val slti : rd:int -> rs1:int -> imm12:int -> Types.I.t
val sltiu : rd:int -> rs1:int -> imm12:int -> Types.I.t
val xori : rd:int -> rs1:int -> imm12:int -> Types.I.t
val ori : rd:int -> rs1:int -> imm12:int -> Types.I.t
val andi : rd:int -> rs1:int -> imm12:int -> Types.I.t
val add : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sub : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sll : rd:int -> rs1:int -> rs2:int -> Types.I.t
val slt : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sltu : rd:int -> rs1:int -> rs2:int -> Types.I.t
val xor_ : rd:int -> rs1:int -> rs2:int -> Types.I.t
val srl : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sra : rd:int -> rs1:int -> rs2:int -> Types.I.t
val or_ : rd:int -> rs1:int -> rs2:int -> Types.I.t
val and_ : rd:int -> rs1:int -> rs2:int -> Types.I.t
val slliw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val srliw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val sraiw : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val lb : rd:int -> rs1:int -> imm12:int -> Types.I.t
val lh : rd:int -> rs1:int -> imm12:int -> Types.I.t
val lw : rd:int -> rs1:int -> imm12:int -> Types.I.t
val lbu : rd:int -> rs1:int -> imm12:int -> Types.I.t
val lhu : rd:int -> rs1:int -> imm12:int -> Types.I.t
val sb : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val sh : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val sw : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val fence : pred:int -> succ:int -> Types.I.t
val fence_i : Types.I.t
val scall : Types.I.t
val sbreak : Types.I.t
val _rdcycle : rd:int -> Types.I.t
val _rdtime : rd:int -> Types.I.t
val _rdinstret : rd:int -> Types.I.t
val _rdcycleh : rd:int -> Types.I.t
val _rdtimeh : rd:int -> Types.I.t
val _rdinstreth : rd:int -> Types.I.t

end

module Asm : sig

val beq : rs1:int -> rs2:int -> imm:int -> Types.I.t
val bne : rs1:int -> rs2:int -> imm:int -> Types.I.t
val blt : rs1:int -> rs2:int -> imm:int -> Types.I.t
val bge : rs1:int -> rs2:int -> imm:int -> Types.I.t
val bltu : rs1:int -> rs2:int -> imm:int -> Types.I.t
val bgeu : rs1:int -> rs2:int -> imm:int -> Types.I.t
val jalr : rd:int -> rs1:int -> imm:int -> Types.I.t
val jal : rd:int -> imm:int -> Types.I.t
val lui : rd:int -> imm:int -> Types.I.t
val auipc : rd:int -> imm:int -> Types.I.t
val addi : rd:int -> rs1:int -> imm:int -> Types.I.t
val slti : rd:int -> rs1:int -> imm:int -> Types.I.t
val sltiu : rd:int -> rs1:int -> imm:int -> Types.I.t
val xori : rd:int -> rs1:int -> imm:int -> Types.I.t
val ori : rd:int -> rs1:int -> imm:int -> Types.I.t
val andi : rd:int -> rs1:int -> imm:int -> Types.I.t
val add : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sub : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sll : rd:int -> rs1:int -> rs2:int -> Types.I.t
val slt : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sltu : rd:int -> rs1:int -> rs2:int -> Types.I.t
val xor_ : rd:int -> rs1:int -> rs2:int -> Types.I.t
val srl : rd:int -> rs1:int -> rs2:int -> Types.I.t
val sra : rd:int -> rs1:int -> rs2:int -> Types.I.t
val or_ : rd:int -> rs1:int -> rs2:int -> Types.I.t
val and_ : rd:int -> rs1:int -> rs2:int -> Types.I.t
val slliw : rd:int -> rs1:int -> imm:int -> Types.I.t
val srliw : rd:int -> rs1:int -> imm:int -> Types.I.t
val sraiw : rd:int -> rs1:int -> imm:int -> Types.I.t
val lb : rd:int -> rs1:int -> imm:int -> Types.I.t
val lh : rd:int -> rs1:int -> imm:int -> Types.I.t
val lw : rd:int -> rs1:int -> imm:int -> Types.I.t
val lbu : rd:int -> rs1:int -> imm:int -> Types.I.t
val lhu : rd:int -> rs1:int -> imm:int -> Types.I.t
val sb : rs1:int -> rs2:int -> imm:int -> Types.I.t
val sh : rs1:int -> rs2:int -> imm:int -> Types.I.t
val sw : rs1:int -> rs2:int -> imm:int -> Types.I.t
val fence : pred:int -> succ:int -> Types.I.t
val fence_i : Types.I.t
val scall : Types.I.t
val sbreak : Types.I.t
val _rdcycle : rd:int -> Types.I.t
val _rdtime : rd:int -> Types.I.t
val _rdinstret : rd:int -> Types.I.t
val _rdcycleh : rd:int -> Types.I.t
val _rdtimeh : rd:int -> Types.I.t
val _rdinstreth : rd:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

