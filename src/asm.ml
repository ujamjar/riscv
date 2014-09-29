(* EDSL for writing assembly in OCaml *)

(* XXX All of this sort stuff should be derived from some common source *)

open Types

(* basic instruction types *)

let r_type ~opcode ~rd ~funct3 ~rs1 ~rs2 ~funct7 = 
  I.(
    ((opcode &: mask 7) <<:  0) |:
    ((rd     &: mask 5) <<:  7) |:
    ((funct3 &: mask 3) <<: 12) |:
    ((rs1    &: mask 5) <<: 15) |:
    ((rs2    &: mask 5) <<: 20) |:
    ((funct7 &: mask 7) <<: 25) 
  )

let i_type ~opcode ~rd ~funct3 ~rs1 ~imm = 
  I.(
    ((opcode &: mask  7) <<:  0) |:
    ((rd     &: mask  5) <<:  7) |:
    ((funct3 &: mask  3) <<: 12) |:
    ((rs1    &: mask  5) <<: 15) |:
    ((imm    &: mask 12) <<: 20) 
  )

let s_type ~opcode ~funct3 ~rs1 ~rs2 ~imm = 
  let imm_5_11 = I.(imm >>: 5) in
  I.(
    ((opcode   &: mask 7) <<:  0) |:
    ((imm      &: mask 5) <<:  7) |:
    ((funct3   &: mask 3) <<: 12) |:
    ((rs1      &: mask 5) <<: 15) |:
    ((rs2      &: mask 5) <<: 20) |:
    ((imm_5_11 &: mask 7) <<: 25) 
  )

let sb_type ~opcode ~funct3 ~rs1 ~rs2 ~imm = 
  let imm_11   = I.(imm >>: 11) in
  let imm_4_1  = I.(imm >>:  1) in
  let imm_10_5 = I.(imm >>:  5) in
  let imm_12   = I.(imm >>: 12) in
  I.(
    ((opcode   &: mask 7) <<:  0) |:
    ((imm_11   &: mask 1) <<:  7) |:
    ((imm_4_1  &: mask 4) <<:  8) |:
    ((funct3   &: mask 3) <<: 12) |:
    ((rs1      &: mask 5) <<: 15) |:
    ((rs2      &: mask 5) <<: 20) |:
    ((imm_10_5 &: mask 6) <<: 25) |:
    ((imm_12   &: mask 1) <<: 30) 
  )

let u_type ~opcode ~rd ~imm = 
  let imm = I.(imm >>: 12) in
  I.(
    ((opcode &: mask  7) <<:  0) |:
    ((rd     &: mask  5) <<:  7) |:
    ((imm    &: mask 20) <<: 12) 
  )

let uj_type ~opcode ~rd ~imm =
  let imm_14_12 = I.(imm >>: 12) in
  let imm_19_15 = I.(imm >>: 15) in
  let imm_11    = I.(imm >>: 11) in
  let imm_4_1   = I.(imm >>:  1) in
  let imm_10_5  = I.(imm >>:  5) in
  let imm_20    = I.(imm >>: 20) in
  I.(
    ((opcode    &: mask  7) <<:  0) |:
    ((rd        &: mask  5) <<:  7) |:
    ((imm_14_12 &: mask  3) <<: 12) |:
    ((imm_19_15 &: mask  5) <<: 15) |:
    ((imm_11    &: mask  1) <<: 20) |:
    ((imm_4_1   &: mask  4) <<: 21) |:
    ((imm_10_5  &: mask  6) <<: 25) |:
    ((imm_20    &: mask  1) <<: 31) 
  )

let sh_type5 ~opcode ~rd ~funct3 ~rs1 ~shamt ~funct7 = 
  I.(
    ((opcode &: mask 7) <<:  0) |:
    ((rd     &: mask 5) <<:  7) |:
    ((funct3 &: mask 3) <<: 12) |:
    ((rs1    &: mask 5) <<: 15) |:
    ((shamt  &: mask 5) <<: 20) |:
    ((funct7 &: mask 7) <<: 25) 
  )

let sh_type6 ~opcode ~rd ~funct3 ~rs1 ~shamt ~funct6 = 
  I.(
    ((opcode &: mask 7) <<:  0) |:
    ((rd     &: mask 5) <<:  7) |:
    ((funct3 &: mask 3) <<: 12) |:
    ((rs1    &: mask 5) <<: 15) |:
    ((shamt  &: mask 6) <<: 20) |:
    ((funct6 &: mask 6) <<: 25) 
  )

let r4_type ~opcode ~rd ~funct3 ~rs1 ~rs2 ~funct2 ~rs3 = 
  I.(
    ((opcode &: mask 7) <<:  0) |:
    ((rd     &: mask 5) <<:  7) |:
    ((funct3 &: mask 3) <<: 12) |:
    ((rs1    &: mask 5) <<: 15) |:
    ((rs2    &: mask 5) <<: 20) |:
    ((funct2 &: mask 2) <<: 25) |:
    ((rs3    &: mask 5) <<: 27) 
  )

type r_type  = rd:I.t -> rs1:I.t -> rs2:I.t -> I.t
type i_type  = rd:I.t -> rs1:I.t -> imm:I.t -> I.t
type s_type  = rs1:I.t -> rs2:I.t -> imm:I.t -> I.t
type sb_type = rs1:I.t -> rs2:I.t -> imm:I.t -> I.t
type u_type  = rd:I.t -> imm:I.t -> I.t
type uj_type = rd:I.t -> imm:I.t -> I.t
type sh_type  = rd:I.t -> rs1:I.t -> shamt:I.t -> I.t
type atomicl = rl:I.t -> aq:I.t -> rd:I.t -> rs1:I.t -> I.t
type atomic = rl:I.t -> aq:I.t -> rd:I.t -> rs1:I.t -> rs2:I.t -> I.t
type f_type1 = rm:I.t -> rd:I.t -> rs1:I.t -> rs2:I.t -> rs3:I.t -> I.t
type f_type2 = rm:I.t -> rd:I.t -> rs1:I.t -> rs2:I.t -> I.t
type f_type3 = rm:I.t -> rd:I.t -> rs1:I.t -> I.t
type f_type4 = rd:I.t -> rs1:I.t -> I.t
type f_type5 = rd:I.t -> I.t

(* RV32I (/ RV64I) base instruction set *)
module RV32I = struct

  let lui = u_type ~opcode:0b0110111l
  let auipc = u_type ~opcode:0b0010111l
  let jal = uj_type ~opcode:0b1101111l
  let jalr = i_type ~opcode:0b1100111l ~funct3:0b000l

  let beq = sb_type ~opcode:0b1100011l ~funct3:0b000l
  let bne = sb_type ~opcode:0b1100011l ~funct3:0b001l
  let blt = sb_type ~opcode:0b1100011l ~funct3:0b100l
  let bge = sb_type ~opcode:0b1100011l ~funct3:0b101l
  let bltu = sb_type ~opcode:0b1100011l ~funct3:0b110l
  let bgeu = sb_type ~opcode:0b1100011l ~funct3:0b111l

  let lb = i_type ~opcode:0b0000011l ~funct3:0b000l
  let lh = i_type ~opcode:0b0000011l ~funct3:0b001l
  let lw = i_type ~opcode:0b0000011l ~funct3:0b010l
  let lbu = i_type ~opcode:0b0000011l ~funct3:0b100l
  let lhu = i_type ~opcode:0b0000011l ~funct3:0b101l

  let sb = s_type ~opcode:0b0100011l ~funct3:0b000l
  let sh = s_type ~opcode:0b0100011l ~funct3:0b001l
  let sw = s_type ~opcode:0b0100011l ~funct3:0b010l

  let addi = i_type ~opcode:0b0010011l ~funct3:0b000l
  let slti = i_type ~opcode:0b0010011l ~funct3:0b010l
  let sltiu = i_type ~opcode:0b0010011l ~funct3:0b011l
  let xori = i_type ~opcode:0b0010011l ~funct3:0b100l
  let ori = i_type ~opcode:0b0010011l ~funct3:0b110l
  let andi = i_type ~opcode:0b0010011l ~funct3:0b111l

  let slli = sh_type5 ~opcode:0b0010011l ~funct3:0b001l ~funct7:0b0000000l
  let srli = sh_type5 ~opcode:0b0010011l ~funct3:0b101l ~funct7:0b0000000l
  let srai = sh_type5 ~opcode:0b0010011l ~funct3:0b101l ~funct7:0b0100000l
  let add = r_type ~opcode:0b0010011l ~funct3:0b000l ~funct7:0b0000000l
  let sub = r_type ~opcode:0b0010011l ~funct3:0b000l ~funct7:0b0100000l
  let sll = r_type ~opcode:0b0010011l ~funct3:0b001l ~funct7:0b0000000l
  let slt = r_type ~opcode:0b0010011l ~funct3:0b010l ~funct7:0b0000000l
  let sltu = r_type ~opcode:0b0010011l ~funct3:0b011l ~funct7:0b0000000l
  let xor = r_type ~opcode:0b0010011l ~funct3:0b100l ~funct7:0b0000000l
  let srl = r_type ~opcode:0b0010011l ~funct3:0b101l ~funct7:0b0000000l
  let sra = r_type ~opcode:0b0010011l ~funct3:0b101l ~funct7:0b0100000l
  let or_ = r_type ~opcode:0b0010011l ~funct3:0b110l ~funct7:0b0000000l
  let and_ = r_type ~opcode:0b0010011l ~funct3:0b111l ~funct7:0b0000000l

  let fence ~pred ~succ = 
    i_type ~opcode:0b0001111l ~funct3:0l ~rd:0l ~rs1:0l ~imm:I.(succ |: (pred <<: 4))
  let fencei = i_type ~opcode:0b0001111l ~funct3:1l ~rd:0l ~rs1:0l ~imm:0l

  let scall = i_type ~opcode:0b1110011l ~funct3:0l ~rd:0l ~rs1:0l ~imm:0l
  let sbreak = i_type ~opcode:0b1110011l ~funct3:0l ~rd:0l ~rs1:0l ~imm:1l
  let rdcycle = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b110000000000l
  let rdcycleh = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b110010000000l
  let rdtime = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b110000000001l
  let rdtimeh = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b110010000001l
  let rdinstret = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b110000000010l
  let rdinstreth = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b110010000010l

end

(* RV64I *)

module RV64I = struct

  let lwu = i_type ~opcode:0b0000011l ~funct3:0b110l
  let ld = i_type ~opcode:0b0000011l ~funct3:0b011l
  let sd = s_type ~opcode:0b0100011l ~funct3:0b011l

  let slli = sh_type6 ~opcode:0b0010011l ~funct3:0b001l ~funct6:0b000000l
  let srli = sh_type6 ~opcode:0b0010011l ~funct3:0b101l ~funct6:0b000000l
  let srai = sh_type6 ~opcode:0b0010011l ~funct3:0b101l ~funct6:0b010000l

  let addiw = i_type ~opcode:0b0011011l ~funct3:0b000l

  let slliw = sh_type5 ~opcode:0b0011011l ~funct3:0b001l ~funct7:0b0000000l
  let srliw = sh_type5 ~opcode:0b0011011l ~funct3:0b101l ~funct7:0b0000000l
  let sraiw = sh_type5 ~opcode:0b0011011l ~funct3:0b101l ~funct7:0b0100000l

  let addw = r_type ~opcode:0b0111011l ~funct3:0b000l ~funct7:0b0000000l
  let subw = r_type ~opcode:0b0111011l ~funct3:0b000l ~funct7:0b0100000l
  let sllw = r_type ~opcode:0b0111011l ~funct3:0b001l ~funct7:0b0000000l
  let srlw = r_type ~opcode:0b0111011l ~funct3:0b101l ~funct7:0b0000000l
  let sraw = r_type ~opcode:0b0111011l ~funct3:0b101l ~funct7:0b0100000l

end

module RV32M = struct
  let mul = r_type ~opcode:0b0110011l ~funct3:0b000l ~funct7:0b0000001l
  let mulh = r_type ~opcode:0b0110011l ~funct3:0b001l ~funct7:0b0000001l
  let mulhsu = r_type ~opcode:0b0110011l ~funct3:0b010l ~funct7:0b0000001l
  let mulhu = r_type ~opcode:0b0110011l ~funct3:0b011l ~funct7:0b0000001l
  let div = r_type ~opcode:0b0110011l ~funct3:0b100l ~funct7:0b0000001l
  let divu = r_type ~opcode:0b0110011l ~funct3:0b101l ~funct7:0b0000001l
  let rem = r_type ~opcode:0b0110011l ~funct3:0b110l ~funct7:0b0000001l
  let remu = r_type ~opcode:0b0110011l ~funct3:0b111l ~funct7:0b0000001l
end

module RV64M = struct
  let mulw = r_type ~opcode:0b0111011l ~funct3:0b000l ~funct7:0b0000001l
  let divw = r_type ~opcode:0b0111011l ~funct3:0b100l ~funct7:0b0000001l
  let divuw = r_type ~opcode:0b0111011l ~funct3:0b101l ~funct7:0b0000001l
  let remw = r_type ~opcode:0b0111011l ~funct3:0b110l ~funct7:0b0000001l
  let remuw = r_type ~opcode:0b0111011l ~funct3:0b111l ~funct7:0b0000001l
end

module RV32A = struct
  let atomic ~rl ~aq ~funct5 = r_type ~opcode:0b0101111l ~funct3:0b010l
    ~funct7:I.(rl |: (aq <<: 1) |: (funct5 <<: 2))
  let lrw = atomic ~funct5:0b00010l ~rs2:0b00000l
  let scw = atomic ~funct5:0b00011l 
  let amoswapw = atomic ~funct5:0b00001l 
  let amoaddw = atomic ~funct5:0b00000l 
  let amoxorw = atomic ~funct5:0b00100l 
  let amoandw = atomic ~funct5:0b01100l 
  let amoorw = atomic ~funct5:0b01000l 
  let amominw = atomic ~funct5:0b10000l 
  let amomaxw = atomic ~funct5:0b10100l 
  let amominuw = atomic ~funct5:0b11000l 
  let amomaxuw = atomic ~funct5:0b11100l 
end

module RV64A = struct
  let atomic ~rl ~aq ~funct5 = r_type ~opcode:0b0101111l ~funct3:0b011l
    ~funct7:I.(rl |: (aq <<: 1) |: (funct5 <<: 2))
  let lrd = atomic ~funct5:0b00010l ~rs2:0b00000l
  let scd = atomic ~funct5:0b00011l 
  let amoswapd = atomic ~funct5:0b00001l 
  let amoaddd = atomic ~funct5:0b00000l 
  let amoxord = atomic ~funct5:0b00100l 
  let amoandd = atomic ~funct5:0b01100l 
  let amoord = atomic ~funct5:0b01000l 
  let amomind = atomic ~funct5:0b10000l 
  let amomaxd = atomic ~funct5:0b10100l 
  let amominud = atomic ~funct5:0b11000l 
  let amomaxud = atomic ~funct5:0b11100l 
end

module RV32F = struct
  let flw = i_type ~opcode:0b0000111l ~funct3:0b010l
  let fsw = s_type ~opcode:0b0100111l ~funct3:0b010l

  let fmadds ~rm = r4_type ~opcode:0b1000011l ~funct3:rm ~funct2:0b00l
  let fmsubs ~rm = r4_type ~opcode:0b1000111l ~funct3:rm ~funct2:0b00l
  let fnmsubs ~rm = r4_type ~opcode:0b1001011l ~funct3:rm ~funct2:0b00l
  let fnmadds ~rm = r4_type ~opcode:0b1001111l ~funct3:rm ~funct2:0b00l

  let fadds ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0000000l
  let fsubs ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0000100l
  let fmuls ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0001000l
  let fdivs ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0001100l

  let fsqrts ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0101100l ~rs2:0b00000l

  let fsgnjs = r_type ~opcode:0b1010011l ~funct3:0b000l ~funct7:0b0010000l
  let fsgnjns = r_type ~opcode:0b1010011l ~funct3:0b001l ~funct7:0b0010000l
  let fsgnjxs = r_type ~opcode:0b1010011l ~funct3:0b010l ~funct7:0b0010000l
  let fmins = r_type ~opcode:0b1010011l ~funct3:0b000l ~funct7:0b0010100l
  let fmaxs = r_type ~opcode:0b1010011l ~funct3:0b001l ~funct7:0b0010100l

  let fcvtws ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100000l ~rs2:0b00000l
  let fcvtwus ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100000l ~rs2:0b00001l

  let fmvxs = r_type ~opcode:0b1010011l ~funct3:0b000l ~rs2:0b00000l ~funct7:0b1110000l
  
  let feqs = r_type ~opcode:0b1010011l ~funct3:0b010l ~funct7:0b1010000l
  let flts = r_type ~opcode:0b1010011l ~funct3:0b001l ~funct7:0b1010000l
  let fles = r_type ~opcode:0b1010011l ~funct3:0b000l ~funct7:0b1010000l

  let fclasss = r_type ~opcode:0b1010011l ~funct3:0b001l ~rs2:0b00000l ~funct7:0b1110000l

  let fcvtsw ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101000l ~rs2:0b00000l
  let fcvtswu ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101000l ~rs2:0b00001l

  let fmvsx = r_type ~opcode:0b1010011l ~funct3:0b000l ~rs2:0b00000l ~funct7:0b1111000l

  let frcsr = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b000000000011l
  let frrm = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b000000000010l
  let frflags = i_type ~opcode:0b1110011l ~funct3:0b010l ~rs1:0l ~imm:0b000000000001l

  let fscsr = i_type ~opcode:0b1110011l ~funct3:0b001l ~imm:0b000000000011l
  let fsrm = i_type ~opcode:0b1110011l ~funct3:0b001l ~imm:0b000000000010l
  let fsflags = i_type ~opcode:0b1110011l ~funct3:0b001l ~imm:0b000000000001l

  let fsrmi = i_type ~opcode:0b1110011l ~funct3:0b101l ~rs1:0l ~imm:0b000000000010l
  let fsflagsi = i_type ~opcode:0b1110011l ~funct3:0b101l ~rs1:0l ~imm:0b000000000001l
end

module RV64F = struct
  let fcvtls ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100000l ~rs2:0b00010l
  let fcvtlus ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100000l ~rs2:0b00011l
  let fcvtsl ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101000l ~rs2:0b00010l
  let fcvtslu ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101000l ~rs2:0b00011l
end

module RV32D = struct
  let fld = i_type ~opcode:0b0000111l ~funct3:0b011l
  let fsd = s_type ~opcode:0b0100111l ~funct3:0b011l

  let fmaddd ~rm = r4_type ~opcode:0b1000011l ~funct3:rm ~funct2:0b01l
  let fmsubd ~rm = r4_type ~opcode:0b1000111l ~funct3:rm ~funct2:0b01l
  let fnmsubd ~rm = r4_type ~opcode:0b1001011l ~funct3:rm ~funct2:0b01l
  let fnmaddd ~rm = r4_type ~opcode:0b1001111l ~funct3:rm ~funct2:0b01l

  let faddd ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0000001l
  let fsubd ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0000101l
  let fmuld ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0001001l
  let fdivd ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0001101l

  let fsqrtd ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0101101l ~rs2:0b00000l

  let fsgnjd = r_type ~opcode:0b1010011l ~funct3:0b000l ~funct7:0b0010001l
  let fsgnjnd = r_type ~opcode:0b1010011l ~funct3:0b001l ~funct7:0b0010001l
  let fsgnjxd = r_type ~opcode:0b1010011l ~funct3:0b010l ~funct7:0b0010001l
  let fmind = r_type ~opcode:0b1010011l ~funct3:0b000l ~funct7:0b0010101l
  let fmaxd = r_type ~opcode:0b1010011l ~funct3:0b001l ~funct7:0b0010101l

  let feqd = r_type ~opcode:0b1010011l ~funct3:0b010l ~funct7:0b1010001l
  let fltd = r_type ~opcode:0b1010011l ~funct3:0b001l ~funct7:0b1010001l
  let fled = r_type ~opcode:0b1010011l ~funct3:0b000l ~funct7:0b1010001l

  let fclassd = r_type ~opcode:0b1010011l ~funct3:0b001l ~rs2:0b00000l ~funct7:0b1110001l

  let fcvtsd ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0100000l ~rs2:0b00001l
  let fcvtds ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b0100001l ~rs2:0b00000l

  let fcvtwd ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100001l ~rs2:0b00000l
  let fcvtwud ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100001l ~rs2:0b00001l
  let fcvtdw ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101001l ~rs2:0b00000l
  let fcvtdwu ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101001l ~rs2:0b00001l

end

module RV64D = struct
  let fcvtld ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100001l ~rs2:0b00010l
  let fcvtlud ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1100001l ~rs2:0b00011l
  let fcvtdl ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101001l ~rs2:0b00010l
  let fcvtdlu ~rm = r_type ~opcode:0b1010011l ~funct3:rm ~funct7:0b1101001l ~rs2:0b00011l
  let fmvxd = r_type ~opcode:0b1010011l ~funct3:0b000l ~rs2:0b00000l ~funct7:0b1110001l
  let fmvdx = r_type ~opcode:0b1010011l ~funct3:0b000l ~rs2:0b00000l ~funct7:0b1111001l
end

module RV32G = struct
  include RV32I
  include RV32M
  include RV32A
  include RV32F
  include RV32D
end

module RV64G = struct
  include RV32G
  include RV64I
  include RV64M
  include RV64A
  include RV64F
  include RV64D
end

