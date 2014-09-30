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
| `slli
| `slti
| `sltiu
| `xori
| `srli
| `srai
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
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

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
val slli : rd:int -> rs1:int -> shamt:int -> Types.I.t
val slti : rd:int -> rs1:int -> imm12:int -> Types.I.t
val sltiu : rd:int -> rs1:int -> imm12:int -> Types.I.t
val xori : rd:int -> rs1:int -> imm12:int -> Types.I.t
val srli : rd:int -> rs1:int -> shamt:int -> Types.I.t
val srai : rd:int -> rs1:int -> shamt:int -> Types.I.t
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

end

