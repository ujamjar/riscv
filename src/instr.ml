(* instruction decode and execute *)
open Types

exception RISCV_illegal_instruction
exception RISCV_floating_point_disabled
exception RISCV_instruction_not_yet_implemented
exception RISCV_system_call
exception RISCV_breakpoint

module Make(T : T) = struct

  open T

  module Mem = Mem.Make(T)

  let opcode = I.extract_bits ~pos:0 ~size:7

  let rd i = I.extract_bits ~pos:7 ~size:5 i 
  let rs1 i = I.extract_bits ~pos:15 ~size:5 i
  let rs2 i = I.extract_bits ~pos:20 ~size:5 i

  let funct3 i = I.extract_bits ~pos:12 ~size:3 i
  let funct7 i = I.extract_bits ~pos:25 ~size:7 i
  let funct10 i = I.((funct7 i <<: 3) |: (funct3 i))
  let superfunct i = I.((funct3 i) |: ((rs2 i) <<: 3) |: ((funct7 i) <<: 8)) 
  let funct8 i = I.(((funct7 i >>: 3) <<: 2) |: (funct3 i))

  let csr_imm i = I.(i >>: 20)
  let i_imm i = I.(i >>+ 20)
  let s_imm i = I.(((i >>+ 20) &: 0xFFFFFFE0l) |: ((i >>: 7) &: 0x1Fl))
  let b_imm i = 
    let t = s_imm i in
    I.((t &: 0xFFFFF7FEl) |: ((t &: one) <<: 11))
  let u_imm i = I.(i &: 0xFFFFF000l)
  let j_imm i = 
    let t = I.((i >>+ 20) &: 0xFFF007FEl) in
    I.(t |: ((i >>: 9) &: 0x800l) |: (i &: 0xFF000l))

  let sexti pos v = 
    let pos = I.bits - pos - 1 in
    I.((v <<: pos) >>+ pos)
  let sextd pos v = 
    let pos = D.bits - pos - 1 in
    D.((v <<: pos) >>+ pos)

  let mask_dlbits = D.mask D.lbits
  let four = D.(one <<: 2)

  let i_type riscv instr = 
    let rd = I.to_int (rd instr) in
    let rs1 = I.to_int (rs1 instr) in
    let i_imm = D.of_i (sexti 31 (i_imm instr)) in

    match funct3 instr with
    (* ADDI *)
    | 0x0l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) +: i_imm)
    (* SLLI *)
    | 0x1l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) <<: (to_int (i_imm &: mask_dlbits)))
    (* SLTI *)
    | 0x2l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) <+ i_imm)
    (* SLTIU *)
    | 0x3l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) <: i_imm)
    (* XOR *)
    | 0x4l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) ^: i_imm)
    (* SRLI and SRAI *)
    | 0x5l -> 
      riscv.regs.(rd) <- 
        if D.(i_imm >>: 6 = D.zero) then 
          D.(riscv.regs.(rs1) >>: (to_int (i_imm &: mask_dlbits)))
        else 
          D.(riscv.regs.(rs1) >>+ (to_int (i_imm &: mask_dlbits)))
    (* ORI *)
    | 0x6l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) |: i_imm)
    (* ANDI *)
    | 0x7l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) &: i_imm)
    | _ -> raise RISCV_illegal_instruction

  let r_type riscv instr = 
    let rd = I.to_int (rd instr) in
    let rs1 = I.to_int (rs1 instr) in
    let rs2 = I.to_int (rs2 instr) in

    match funct10 instr with
    (* ADD *)
    | 0x0l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) +: riscv.regs.(rs2))
    (* SUB *)
    | 0x100l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) -: riscv.regs.(rs2))
    (* SLL *)
    | 0x1l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) <<: (to_int (riscv.regs.(rs2) &: mask_dlbits)))
    (* SLT *)
    | 0x2l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) <+ riscv.regs.(rs2))
    (* SLTU *)
    | 0x3l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) <: riscv.regs.(rs2))
    (* XOR *)
    | 0x4l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) ^: riscv.regs.(rs2))
    (* SRL *)
    | 0x5l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) >>: (to_int (riscv.regs.(rs2) &: mask_dlbits)))
    (* SRA *)
    | 0x105l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) >>+ (to_int (riscv.regs.(rs2) &: mask_dlbits)))
    (* OR *)
    | 0x6l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) |: riscv.regs.(rs2))
    (* AND *)
    | 0x7l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) &: riscv.regs.(rs2))
    (* MUL *)
    | 0x8l -> 
      riscv.regs.(rd) <- D.(riscv.regs.(rs1) *: riscv.regs.(rs2))
    (* MULH *)
    | 0x9l -> raise RISCV_instruction_not_yet_implemented
    (* MULHSU *)
    | 0xAl -> raise RISCV_instruction_not_yet_implemented
    (* MULHU *)
    | 0xBl -> raise RISCV_instruction_not_yet_implemented
    (* DIV *)
    | 0xCl -> raise RISCV_instruction_not_yet_implemented
    (* DIVU *)
    | 0xDl -> raise RISCV_instruction_not_yet_implemented
    (* REM *)
    | 0xEl -> raise RISCV_instruction_not_yet_implemented
    (* REMU *)
    | 0xFl -> raise RISCV_instruction_not_yet_implemented
    | _ -> raise RISCV_illegal_instruction

  let lui riscv instr = 
    let rd = I.to_int (rd instr) in
    let u_imm = D.of_i (u_imm instr) in
    riscv.regs.(rd) <- u_imm 

  let auipc riscv instr = 
    let rd = I.to_int (rd instr) in
    let u_imm = D.of_i (u_imm instr) in
    riscv.regs.(rd) <- D.(u_imm +: riscv.pc)

  let jal riscv instr = 
    let rd = I.to_int (rd instr) in
    let j_imm = D.of_i (j_imm instr) in
    riscv.regs.(rd) <- D.(riscv.pc +: four);
    riscv.pc <- D.(j_imm +: riscv.pc)

  let b_type riscv instr =  
    let rs1 = I.to_int (rs1 instr) in
    let rs2 = I.to_int (rs2 instr) in
    let b_imm = D.of_i (b_imm instr) in
    let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in
    match funct3 instr with
    (* BEQ *)
    | 0x0l -> 
      if riscv.regs.(rs1) = riscv.regs.(rs2) then riscv.pc <- D.(riscv.pc +: b_imm)
      else incr_pc()
    (* BNE *)
    | 0x1l -> 
      if riscv.regs.(rs1) <> riscv.regs.(rs2) then riscv.pc <- D.(riscv.pc +: b_imm)
      else incr_pc()
    (* BLT *)
    | 0x4l -> 
      if D.(riscv.regs.(rs1) <+ riscv.regs.(rs2)) = D.one then riscv.pc <- D.(riscv.pc +: b_imm)
      else incr_pc()
    (* BGE *)
    | 0x5l -> 
      if D.(riscv.regs.(rs1) >=+ riscv.regs.(rs2)) = D.one then riscv.pc <- D.(riscv.pc +: b_imm)
      else incr_pc()
    (* BLTU *)
    | 0x6l -> 
      if D.(riscv.regs.(rs1) <: riscv.regs.(rs2)) = D.one then riscv.pc <- D.(riscv.pc +: b_imm)
      else incr_pc()
    (* BGEU *)
    | 0x7l -> 
      if D.(riscv.regs.(rs1) >=: riscv.regs.(rs2)) = D.one then riscv.pc <- D.(riscv.pc +: b_imm)
      else incr_pc()
    | _ -> raise RISCV_illegal_instruction

  let jalr riscv instr = 
    let rd = I.to_int (rd instr) in
    let rs1 = I.to_int (rs1 instr) in
    let i_imm = D.of_i (sexti 31 (i_imm instr)) in

    match funct3 instr with 
    | 0x0l -> begin
      riscv.regs.(rd) <- D.(riscv.pc +: four);
      riscv.pc <- D.(i_imm +: riscv.regs.(rs1))
    end
    | _ -> raise RISCV_illegal_instruction

  let loads riscv instr = 
    (* XXX address translation *)
    let rd = I.to_int (rd instr) in
    let rs1 = I.to_int (rs1 instr) in
    let i_imm = D.of_i (sexti 31 (i_imm instr)) in
    let addr = D.(to_int (riscv.regs.(rs1) +: i_imm)) in

    match funct3 instr with 
    (* LB *)
    | 0x0l -> 
      riscv.regs.(rd) <- sextd 7 (Mem.load 0 riscv.mem addr)
    (* LH *)
    | 0x1l -> 
      riscv.regs.(rd) <- sextd 16 (Mem.load 1 riscv.mem addr)
    (* LW *)
    | 0x2l -> 
      riscv.regs.(rd) <- sextd 32 (Mem.load 2 riscv.mem addr)
    (* LD *)
    | 0x3l -> 
      riscv.regs.(rd) <- Mem.load 3 riscv.mem addr
    (* LBU *)
    | 0x4l -> 
      riscv.regs.(rd) <- Mem.load 0 riscv.mem addr
    (* LHU *)
    | 0x5l -> 
      riscv.regs.(rd) <- Mem.load 1 riscv.mem addr
    (* LWU *)
    | 0x6l -> 
      riscv.regs.(rd) <- Mem.load 2 riscv.mem addr
    | _ -> raise RISCV_illegal_instruction

  let stores riscv instr = 
    let rs1 = I.to_int (rs1 instr) in
    let rs2 = I.to_int (rs2 instr) in
    let i_imm = D.of_i (sexti 31 (i_imm instr)) in
    let addr = D.(to_int (riscv.regs.(rs1) +: i_imm)) in
    let data = riscv.regs.(rs2) in
    match funct3 instr with
    (* SB *)
    | 0x0l -> Mem.store 0 riscv.mem addr data
    (* SH *)
    | 0x1l -> Mem.store 1 riscv.mem addr data
    (* SW *)
    | 0x2l -> Mem.store 2 riscv.mem addr data
    (* SD *)
    | 0x3l -> Mem.store 3 riscv.mem addr data
    | _ -> raise RISCV_illegal_instruction

  let fence riscv instr = raise RISCV_instruction_not_yet_implemented  

  let csr riscv instr = raise RISCV_instruction_not_yet_implemented 

  let sys riscv instr = 
    let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in
    match superfunct instr with
    (* SCALL *)
    | 0x0l -> raise RISCV_system_call
    (* SBREAK *)
    | 0x8l -> raise RISCV_breakpoint
    (* SRET *)
    | 0x4000l -> begin
      let sr = riscv.priv.status in
      let sr = D.(if (sr &: SR.ps) <> zero then sr |: SR.s else sr &: (~: SR.s)) in
      let sr = D.(if (sr &: SR.pei) <> zero then sr |: SR.ei else sr &: (~: SR.ei)) in
      riscv.priv.status <- sr;
      riscv.pc <- riscv.priv.epc
    end
    (* RDCYCLE *)
    | 0x6002l -> begin
      let rd = I.to_int (rd instr) in
      riscv.regs.(rd) <- riscv.priv.cycle;
      incr_pc()
    end
    (* RDTIME *)
    | 0x600al -> begin
      let time = D.of_float (Unix.gettimeofday () *. 1000.) in
      let rd = I.to_int (rd instr) in
      riscv.regs.(rd) <- D.(time -: riscv.priv.time);
      incr_pc()
    end
    (* RDINSTRET *)
    | 0x6012l -> begin
      let rd = I.to_int (rd instr) in
      riscv.regs.(rd) <- riscv.priv.instret;
      incr_pc()
    end
    | _ -> csr riscv instr

  let int32 riscv instr =
    let rd = I.to_int (rd instr) in
    let rs1 = I.to_int (rs1 instr) in
    let i_imm = D.of_i (sexti 31 (i_imm instr)) in
    match funct3 instr with
    (* ADDIW *)
    | 0x0l -> 
      riscv.regs.(rd) <- sextd 31 D.(riscv.regs.(rs1) +: i_imm)
    (* SLLIW *)
    | 0x1l -> (* trap on illegal (>32 bit) shift *)
      riscv.regs.(rd) <- sextd 31 D.(riscv.regs.(rs1) <<: (to_int (i_imm &: mask_dlbits)))
    (* SRLIW and SRAIW *)
    | 0x5l -> 
      riscv.regs.(rd) <- sextd 31
        (if D.(i_imm >>: 6 = D.zero) then 
          D.(riscv.regs.(rs1) >>: (to_int (i_imm &: mask_dlbits)))
        else 
          D.(riscv.regs.(rs1) >>+ (to_int (i_imm &: mask_dlbits))))
    | _ -> raise RISCV_illegal_instruction

  let int32_more riscv instr = 
    let rd = I.to_int (rd instr) in
    let rs1 = I.to_int (rs1 instr) in
    let rs2 = I.to_int (rs2 instr) in

    match funct10 instr with
    (* ADDW *)
    | 0x0l -> 
      riscv.regs.(rd) <- sextd 31 D.(riscv.regs.(rs1) +: riscv.regs.(rs2))
    (* SUBW *)
    | 0x100l -> 
      riscv.regs.(rd) <- sextd 31 D.(riscv.regs.(rs1) -: riscv.regs.(rs2))
    (* SLLW *)
    | 0x1l -> 
      riscv.regs.(rd) <- sextd 31 
        D.(riscv.regs.(rs1) <<: (to_int (riscv.regs.(rs2) &: mask_dlbits)))
    (* SRLW *)
    | 0x5l -> 
      riscv.regs.(rd) <- sextd 31 
        D.(riscv.regs.(rs1) >>: (to_int (riscv.regs.(rs2) &: mask_dlbits)))
    (* SRAW *)
    | 0x105l -> 
      riscv.regs.(rd) <- sextd 31 
        D.(riscv.regs.(rs1) >>+ (to_int (riscv.regs.(rs2) &: mask_dlbits)))
    (* MULW *)
    | 0x8l -> raise RISCV_instruction_not_yet_implemented
    (* DIVW *)
    | 0xcl -> raise RISCV_instruction_not_yet_implemented
    (* DIVUW *)
    | 0xdl -> raise RISCV_instruction_not_yet_implemented
    (* REMW *)
    | 0xel -> raise RISCV_instruction_not_yet_implemented
    (* REMUW *)
    | 0xfl -> raise RISCV_instruction_not_yet_implemented
    | _ -> raise RISCV_illegal_instruction

  let atomic riscv instr = 
    match funct8 instr with
    (* AMOADD.W *)
    | 0x2l -> raise RISCV_instruction_not_yet_implemented
    (* AMOSWAP.W *)
    | 0xAl -> raise RISCV_instruction_not_yet_implemented
    (* AMOXOR.W *)
    | 0x22l -> raise RISCV_instruction_not_yet_implemented
    (* AMOAND.W *)
    | 0x62l -> raise RISCV_instruction_not_yet_implemented
    (* AMOOR.W *)
    | 0x42l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMIN.W *)
    | 0x82l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMAX.W *)
    | 0xA2l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMINU.W *)
    | 0xC2l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMAXU.W *)
    | 0xE2l -> raise RISCV_instruction_not_yet_implemented
    (* AMOADD.D *)
    | 0x3l -> raise RISCV_instruction_not_yet_implemented
    (* AMOSWAP.D *)
    | 0xBl -> raise RISCV_instruction_not_yet_implemented
    (* AMOXOR.D *)
    | 0x23l -> raise RISCV_instruction_not_yet_implemented
    (* AMOAND.D *)
    | 0x63l -> raise RISCV_instruction_not_yet_implemented
    (* AMOOR.D *)
    | 0x43l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMIN.D *)
    | 0x83l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMAX.D *)
    | 0xA3l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMINU.D *)
    | 0xC3l -> raise RISCV_instruction_not_yet_implemented
    (* AMOMAXU.D *)
    | 0xE3l -> raise RISCV_instruction_not_yet_implemented
    (* AMOLR.W *)
    | 0x12l -> raise RISCV_instruction_not_yet_implemented
    (* AMOLR.D *)
    | 0x13l -> raise RISCV_instruction_not_yet_implemented
    (* AMOSC.W *)
    | 0x1Al -> raise RISCV_instruction_not_yet_implemented
    (* AMOSC.D *)
    | 0x1Bl -> raise RISCV_instruction_not_yet_implemented
    | _ -> raise RISCV_illegal_instruction

  let execute riscv instr = 

    let op = opcode instr in
    let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in

    match op with
    (* I-type *)
    | 0x13l -> (i_type riscv instr; incr_pc())

    (* R-TYPE *)
    | 0x33l -> (r_type riscv instr; incr_pc())

    (* L-TYPE (LUI) *)
    | 0x37l -> (lui riscv instr; incr_pc())

    (* L-TYPE (AUIPC) *)
    | 0x17l -> (auipc riscv instr; incr_pc())

    (* J-TYPE (JAL) *)
    | 0x6fl -> jal riscv instr

    (* B-TYPE (branches) *)
    | 0x63l -> b_type riscv instr

    (* I-TYPE (JALR) *)
    | 0x67l -> jalr riscv instr

    (* LOADS *)
    | 0x3l -> (loads riscv instr; incr_pc())

    (* STORES *)
    | 0x23l -> (stores riscv instr; incr_pc())

    (* FENCE *)
    | 0xfl -> fence riscv instr

    (* R-TYPE (SYS INSTRUCTIONS) *)
    | 0x73l -> sys riscv instr

    (* Int32 *)
    | 0x1bl -> (int32 riscv instr; incr_pc())

    (* More Int32 *)
    | 0x3bl -> (int32_more riscv instr; incr_pc())

    (* atomic memory *)
    | 0x2fl -> atomic riscv instr

    (* various floating point ops *)
    | 0x7l | 0x27l | 0x43l | 0x47l | 0x4bl | 0x4fl | 0x53l -> raise RISCV_floating_point_disabled

    | _ -> raise RISCV_illegal_instruction

end

