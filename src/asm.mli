(* EDSL for writing assembly in OCaml *)
open Types

val r_type   : opcode:I.t -> rd:I.t -> funct3:I.t -> rs1:I.t -> rs2:I.t -> funct7:I.t -> I.t
val i_type   : opcode:I.t -> rd:I.t -> funct3:I.t -> rs1:I.t -> imm:I.t -> I.t
val s_type   : opcode:I.t -> funct3:I.t -> rs1:I.t -> rs2:I.t -> imm:I.t -> I.t
val sb_type  : opcode:I.t -> funct3:I.t -> rs1:I.t -> rs2:I.t -> imm:I.t -> I.t
val u_type   : opcode:I.t -> rd:I.t -> imm:I.t -> I.t
val uj_type  : opcode:I.t -> rd:I.t -> imm:I.t -> I.t
val sh_type5 : opcode:I.t -> rd:I.t -> funct3:I.t -> rs1:I.t -> shamt:I.t -> funct7:I.t -> I.t
val sh_type6 : opcode:I.t -> rd:I.t -> funct3:I.t -> rs1:I.t -> shamt:I.t -> funct6:I.t -> I.t
val r4_type  : opcode:I.t -> rd:I.t -> funct3:I.t -> rs1:I.t -> rs2:I.t -> funct2:I.t -> rs3:I.t -> I.t

type r_type  = rd:I.t -> rs1:I.t -> rs2:I.t -> I.t
type i_type  = rd:I.t -> rs1:I.t -> imm:I.t -> I.t
type s_type  = rs1:I.t -> rs2:I.t -> imm:I.t -> I.t
type sb_type = rs1:I.t -> rs2:I.t -> imm:I.t -> I.t
type u_type  = rd:I.t -> imm:I.t -> I.t
type uj_type = rd:I.t -> imm:I.t -> I.t
type sh_type = rd:I.t -> rs1:I.t -> shamt:I.t -> I.t
type atomicl = rl:I.t -> aq:I.t -> rd:I.t -> rs1:I.t -> I.t
type atomic  = rl:I.t -> aq:I.t -> rd:I.t -> rs1:I.t -> rs2:I.t -> I.t

type f_type1 = rm:I.t -> rd:I.t -> rs1:I.t -> rs2:I.t -> rs3:I.t -> I.t
type f_type2 = rm:I.t -> rd:I.t -> rs1:I.t -> rs2:I.t -> I.t
type f_type3 = rm:I.t -> rd:I.t -> rs1:I.t -> I.t
type f_type4 = rd:I.t -> rs1:I.t -> I.t
type f_type5 = rd:I.t -> I.t

module RV32I : sig

  val lui : u_type
  val auipc : u_type
  val jal : uj_type
  val jalr : i_type

  val beq : sb_type
  val bne : sb_type
  val blt : sb_type
  val bge : sb_type
  val bltu : sb_type
  val bgeu : sb_type

  val lb : i_type
  val lh : i_type
  val lw : i_type
  val lbu : i_type
  val lhu : i_type

  val sb : s_type
  val sh : s_type
  val sw : s_type

  val slli : sh_type
  val srli : sh_type
  val srai : sh_type
  val add : r_type
  val sub : r_type
  val sll : r_type
  val slt : r_type
  val sltu : r_type
  val xor : r_type
  val srl : r_type
  val sra : r_type
  val or_ : r_type
  val and_ : r_type

  val fence : pred:I.t -> succ:I.t -> I.t
  val fencei : I.t

  val scall : I.t
  val sbreak : I.t
  val rdcycle : rd:I.t -> I.t
  val rdcycleh : rd:I.t -> I.t
  val rdtime : rd:I.t -> I.t
  val rdtimeh : rd:I.t -> I.t
  val rdinstret : rd:I.t -> I.t
  val rdinstreth : rd:I.t -> I.t

end

module RV64I : sig

  val lwu : i_type
  val ld : i_type
  val sd : s_type
  
  val slli : sh_type
  val srli : sh_type
  val srai : sh_type
  
  val addiw : i_type
  
  val slliw : sh_type
  val srliw : sh_type
  val sraiw : sh_type

  val addw : r_type
  val subw : r_type
  val sllw : r_type
  val srlw : r_type
  val sraw : r_type

end

module RV32M : sig
  val mul : r_type
  val mulh : r_type
  val mulhsu : r_type
  val mulhu : r_type
  val div : r_type
  val divu : r_type
  val rem : r_type
  val remu : r_type
end

module RV64M : sig
  val mulw : r_type
  val divw : r_type
  val divuw : r_type
  val remw : r_type
  val remuw : r_type
end

module RV32A : sig
  val lrw : atomicl
  val scw : atomic
  val amoswapw : atomic
  val amoaddw : atomic
  val amoxorw : atomic
  val amoandw : atomic
  val amoorw : atomic
  val amominw : atomic
  val amomaxw : atomic
  val amominuw : atomic
  val amomaxuw : atomic
end

module RV64A : sig
  val lrd : atomicl
  val scd : atomic
  val amoswapd : atomic
  val amoaddd : atomic
  val amoxord : atomic
  val amoandd : atomic
  val amoord : atomic
  val amomind : atomic
  val amomaxd : atomic
  val amominud : atomic
  val amomaxud : atomic
end

module RV32F : sig
  val flw : i_type
  val fsw : s_type
  val fmadds : f_type1
  val fmsubs : f_type1
  val fnmsubs : f_type1
  val fnmadds : f_type1
  val fadds : f_type2
  val fsubs : f_type2
  val fmuls : f_type2
  val fdivs : f_type2
  val fsqrts : f_type3
  val fsgnjs : r_type
  val fsgnjns : r_type
  val fsgnjxs : r_type
  val fmins : r_type
  val fmaxs : r_type
  val fcvtws : f_type3
  val fcvtwus : f_type3
  val fmvxs : f_type4
  val feqs : r_type
  val flts : r_type
  val fles : r_type
  val fclasss : f_type4
  val fcvtsw : f_type3
  val fcvtswu : f_type3
  val fmvsx : f_type4
  val frcsr : f_type5
  val frrm : f_type5
  val frflags : f_type5
  val fscsr : f_type4
  val fsrm : f_type4
  val fsflags: f_type4
  val fsrmi : f_type5
  val fsflagsi : f_type5
end

module RV64F : sig
  val fcvtls : f_type3
  val fcvtlus : f_type3
  val fcvtsl : f_type3
  val fcvtslu : f_type3
end

module RV32D : sig
  val fld : i_type
  val fsd : s_type
  val fmaddd : f_type1
  val fmsubd : f_type1
  val fnmsubd : f_type1
  val fnmaddd : f_type1
  val faddd : f_type2
  val fsubd : f_type2
  val fmuld : f_type2
  val fdivd : f_type2
  val fsqrtd : f_type3
  val fsgnjd : r_type
  val fsgnjnd : r_type
  val fsgnjxd : r_type
  val fmind : r_type
  val fmaxd : r_type
  val feqd : r_type
  val fltd : r_type
  val fled : r_type
  val fclassd : f_type4
  val fcvtsd : f_type3
  val fcvtds : f_type3
  val fcvtwd : f_type3
  val fcvtwud : f_type3
  val fcvtdw : f_type3
  val fcvtdwu : f_type3
end

module RV64D : sig
  val fcvtld : f_type3
  val fcvtlud : f_type3
  val fcvtdl : f_type3
  val fcvtdlu : f_type3
  val fmvxd : f_type4
  val fmvdx : f_type4
end

module RV32G : sig
  include module type of RV32I
  include module type of RV32M
  include module type of RV32A
  include module type of RV32F
  include module type of RV32D
end

module RV64G : sig
  include module type of RV32G
  include module type of RV64I
  include module type of RV64M
  include module type of RV64A
  include module type of RV64F
  include module type of RV64D
end

