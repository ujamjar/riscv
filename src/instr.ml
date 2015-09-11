(* instruction decode and execute *)
open Types
open Cpu

exception RISCV_illegal_instruction
exception RISCV_floating_point_disabled
exception RISCV_instruction_not_yet_implemented
exception RISCV_system_call
exception RISCV_breakpoint

module Util = struct

  type 'a t = ('a * (Int32.t * Int32.t)) list

  (* given an instruction set, compute the set of bits which are always 0 or always 1 *)
  let constant_10 i = 
    let all1, all0 = 
      List.fold_left 
        (fun (all1, all0) (_,(_,m)) -> I.(all1 &: m, all0 |: m)) (-1l,0l) i
    in
    all1, I.(~: all0)

  let constant_mask i = List.fold_left (fun all (_,(m,_)) -> I.(all |: m)) 0l i

  let constant_all instrs = 
    let all1,all0 = constant_10 instrs in
    I.( ~: (all1 |: all0) ) 

  (* given a mask of non-constant bits (ie ~: (all1 |: all0)) it returns
   * the set of bit ranges which need to be decoded *)
  let ranges all = 
    let rec f prev_pos pos = 
      if pos = 32 then
        match prev_pos with
        | None -> []
        | Some(prev_pos) -> [(prev_pos,32-1)]
      else
        if I.(all &: (1l <<: pos)) = 0l then 
          match prev_pos with
          | None -> f prev_pos (pos+1)
          | Some(prev_pos) -> (prev_pos,pos-1) :: f None (pos+1)
        else 
          match prev_pos with
          | None -> f (Some(pos)) (pos+1)
          | Some(_) -> f prev_pos (pos+1)
    in
    f None 0

  module M = Map.Make(Int32)

  let partition instrs (l,h) = 
    let map = List.fold_left
      (fun map ((_,(m',m)) as y) ->
        let msk = I.( (1l <<: (h-l+1)) -: 1l ) in
        let m = I.( (m >>: l) &: msk ) in
        let m' = I.( (m' >>: l) &: msk ) in
        let rec enum i map =
          let add map = 
            try 
              let els = M.find i map in
              M.add i (y :: els) map
            with _ ->
              M.add i [y] map
          in
          if i > msk then map
          else if I.(i &: m') = I.(m &: m') then begin
            enum I.(i +: 1l) (add map)
          end else 
            enum I.(i +: 1l) map
        in
        enum 0l map 

      ) M.empty instrs
    in
    M.bindings map 

  (* partition instructions *)
  let instruction_decoder instrs = 
    let rec f done_mask instrs = 
      match instrs with
      | [] -> (fun _ -> raise Not_found) (* illegal instruction *)
      | [(x,_)] -> (fun _ -> x) (* result *)
      | _ -> begin
        (* (re-)compute ranges on this branch *)
        let ranges = ranges I.((~: done_mask) &: (constant_all instrs &: constant_mask instrs)) in

        match ranges with
        | (l,h) :: _ -> 
          let msk = I.( (1l <<: (h-l+1)) -: 1l ) in
          (* create an array indexed by instr.[h..l] *)
          let partition = partition instrs (l,h) in
          let a = Array.init (1 lsl (h-l+1))
            (fun i -> 
              let instrs = try List.assoc Int32.(of_int i) partition with _ -> [] in
              f I.(done_mask |: (msk <<: l)) instrs)
          in
          (* look up instruction in array *)
          (fun (i:I.t) -> 
            let m = I.( (i >>: l) &: msk ) in
            a.(I.to_int m) i)

        | _ -> begin
          List.iter (fun (_,(_,m)) -> Printf.printf "%.8lx\n%!" m) instrs;
          raise Not_found (* should not happen *)
        end
      end
    in
    f 0l instrs 

  let instruction_decoder_simple instrs i = 
    let rec f = function
      | [] -> raise Not_found
      | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
    in
    f instrs

end

(* TO BE DELETED.  Still a couple of bits related to the supervisor mode
 * which are in here ... and may not be correct *)
module Make_old(T : T) = struct

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

module Make(T : T) = struct

  open T
  module Mem = Mem.Make(T)

  let rd i = I.to_int @@ I.extract_bits ~pos:7 ~size:5 i 
  let rs1 i = I.to_int @@ I.extract_bits ~pos:15 ~size:5 i
  let rs2 i = I.to_int @@ I.extract_bits ~pos:20 ~size:5 i
  let i_imm i = D.of_i @@ I.(i >>+ 20)
  let s_imm i = I.(((i >>+ 20) &: 0xFFFFFFE0l) |: ((i >>: 7) &: 0x1Fl))
  let b_imm i = 
    let t = s_imm i in
    D.of_i @@ I.((t &: 0xFFFFF7FEl) |: ((t &: one) <<: 11))
  let u_imm i = D.of_i @@ I.(i &: 0xFFFFF000l)
  let j_imm i = 
    let t = I.((i >>+ 20) &: 0xFFF007FEl) in
    D.of_i @@ I.(t |: ((i >>: 9) &: 0x800l) |: (i &: 0xFF000l))
  let s_imm i = D.of_i @@ s_imm i

  let sexti pos v = 
    let pos = I.bits - pos - 1 in
    I.((v <<: pos) >>+ pos)
  let sextd pos v = 
    let pos = D.bits - pos - 1 in
    D.((v <<: pos) >>+ pos)

  let mask_dlbits = D.mask D.lbits
  let four = D.(one <<: 2)

  module RV32I = struct

    let exec riscv i opcode = 
      let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in

      match opcode with
      | `beq -> begin
        if riscv.regs.(rs1 i) = riscv.regs.(rs2 i) then 
          riscv.pc <- D.(riscv.pc +: b_imm i)
        else incr_pc()
      end
      | `bne -> begin
        if riscv.regs.(rs1 i) <> riscv.regs.(rs2 i) then 
          riscv.pc <- D.(riscv.pc +: b_imm i)
        else incr_pc()
      end
      | `blt -> begin
        if D.(riscv.regs.(rs1 i) <+ riscv.regs.(rs2 i)) = D.one then 
          riscv.pc <- D.(riscv.pc +: b_imm i)
        else incr_pc()
      end
      | `bge -> begin
        if D.(riscv.regs.(rs1 i) >=+ riscv.regs.(rs2 i)) = D.one then 
          riscv.pc <- D.(riscv.pc +: b_imm i)
        else incr_pc()
      end
      | `bltu -> begin
        if D.(riscv.regs.(rs1 i) <: riscv.regs.(rs2 i)) = D.one then 
          riscv.pc <- D.(riscv.pc +: b_imm i)
        else incr_pc()
      end
      | `bgeu -> begin
        if D.(riscv.regs.(rs1 i) >=: riscv.regs.(rs2 i)) = D.one then 
          riscv.pc <- D.(riscv.pc +: b_imm i)
        else incr_pc()
      end
      | `jalr -> begin
        riscv.regs.(rd i) <- D.(riscv.pc +: four);
        riscv.pc <- D.(i_imm i +: riscv.regs.(rs1 i))
      end
      | `jal -> begin
        riscv.regs.(rd i) <- D.(riscv.pc +: four);
        riscv.pc <- D.(j_imm i +: riscv.pc)
      end
      | `lui -> begin
        riscv.regs.(rd i) <- u_imm i;
        incr_pc()
      end
      | `auipc -> begin
        riscv.regs.(rd i) <- D.(u_imm i +: riscv.pc);
        incr_pc()
      end
      | `addi -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) +: i_imm i);
        incr_pc()
      end
      | `slliw -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) <<: (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `slti -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) <+ i_imm i);
        incr_pc()
      end
      | `sltiu -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) <: i_imm i);
        incr_pc()
      end
      | `xori -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) ^: i_imm i);
        incr_pc()
      end
      | `srliw -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) >>: (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `sraiw -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) >>+ (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `ori -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) |: i_imm i);
        incr_pc()
      end
      | `andi -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) &: i_imm i);
        incr_pc()
      end
      | `add -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) +: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `sub -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) -: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `sll -> begin
        riscv.regs.(rd i) <- 
          D.(riscv.regs.(rs1 i) <<: (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
        incr_pc()
      end
      | `slt -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) <+ riscv.regs.(rs2 i));
        incr_pc()
      end
      | `sltu -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) <: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `xor_ -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) ^: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `srl -> begin
        riscv.regs.(rd i) <- 
          D.(riscv.regs.(rs1 i) >>: (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
        incr_pc()
      end
      | `sra -> begin
        riscv.regs.(rd i) <- 
          D.(riscv.regs.(rs1 i) >>+ (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
        incr_pc()
      end
      | `or_ -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) |: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `and_ -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) &: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `lb -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- sextd 7 (Mem.load 0 riscv.mem addr);
        incr_pc()
      end
      | `lh -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- sextd 15 (Mem.load 1 riscv.mem addr);
        incr_pc()
      end
      | `lw -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- sextd 31 (Mem.load 2 riscv.mem addr);
        incr_pc()
      end
      | `lbu -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- Mem.load 0 riscv.mem addr;
        incr_pc()
      end
      | `lhu -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- Mem.load 1 riscv.mem addr;
        incr_pc()
      end
      | `sb -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        Mem.store 0 riscv.mem addr riscv.regs.(rs2 i);
        incr_pc()
      end
      | `sh -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        Mem.store 1 riscv.mem addr riscv.regs.(rs2 i);
        incr_pc()
      end
      | `sw -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        Mem.store 2 riscv.mem addr riscv.regs.(rs2 i);
        incr_pc()
      end
      | `fence -> begin 
        raise RISCV_instruction_not_yet_implemented
      end
      | `fence_i -> begin 
        raise RISCV_instruction_not_yet_implemented
      end
      | `scall -> begin
        incr_pc(); (* XXX I think... *)
        raise RISCV_system_call
      end
      | `sbreak -> begin
        raise RISCV_breakpoint (* what about PC? *)
      end
      | `_rdcycle
      | `_rdtime
      | `_rdinstret
      | `_rdcycleh
      | `_rdtimeh
      | `_rdinstreth -> raise RISCV_instruction_not_yet_implemented
      | _ -> raise RISCV_illegal_instruction

  end

  module RV32M = struct
    
    let exec riscv i opcode = 
      match opcode with
      | `mul -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `mulh -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `mulhsu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `mulhu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `div -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `divu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `rem -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `remu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction
  
  end

  module RV32A = struct

    let exec riscv i opcode = 
      match opcode with
      | `amoadd_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoxor_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoor_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoand_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amomin_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amomax_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amominu_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amomaxu_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoswap_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `lr_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `sc_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction

  end

  module RV32F = struct

    let exec riscv i opcode = 
      match opcode with
      | `flw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fadd_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsub_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmul_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fdiv_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsgnj_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsgnjn_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsgnjx_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmin_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmax_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsqrt_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fle_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `flt_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `feq_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_w_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_wu_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmv_x_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fclass_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_s_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_s_wu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmv_s_x -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmadd_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmsub_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fnmsub_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fnmadd_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction

  end

  module RV32D = struct

    let exec riscv i opcode = 
      match opcode with
      | `fld -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsd -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fadd_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsub_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmul_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fdiv_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsgnj_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsgnjn_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsgnjx_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmin_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmax_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_s_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_d_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fsqrt_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fle_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `flt_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `feq_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_w_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_wu_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fclass_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_d_w -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_d_wu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmadd_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmsub_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fnmsub_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fnmadd_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction

  end

  module RVSYS = struct

    let exec riscv i opcode = 
      (*let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in*)
      match opcode with
      | `sret -> begin
        let sr = riscv.priv.status in
        let sr = D.(if (sr &: SR.ps) <> zero then sr |: SR.s else sr &: (~: SR.s)) in
        let sr = D.(if (sr &: SR.pei) <> zero then sr |: SR.ei else sr &: (~: SR.ei)) in
        riscv.priv.status <- sr;
        riscv.pc <- riscv.priv.epc
      end

      (* we need to access the various system registers and do atomic read/set/swaps 
       * this implements things like rdcycle, rdtime rdinstret
       * there are also some differences between 32 and 64 bit mode to handle here.
       * ...and we need the blooming spec! *)
      | `csrrw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `csrrs -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `csrrc -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `csrrwi -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `csrrsi -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `csrrci -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction

  end

  module RV64I = struct
    let exec riscv i opcode = 
      let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in
      match opcode with
      | `lwu -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- Mem.load 2 riscv.mem addr;
        incr_pc()
      end
      | `ld -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        riscv.regs.(rd i) <- Mem.load 3 riscv.mem addr;
        incr_pc()
      end
      | `sd -> begin
        let addr = D.(to_int (riscv.regs.(rs1 i) +: i_imm i)) in
        Mem.store 3 riscv.mem addr riscv.regs.(rs2 i);
        incr_pc()
      end
      | `addiw -> begin
        riscv.regs.(rd i) <- sextd 31 D.(riscv.regs.(rs1 i) +: i_imm i);
        incr_pc()
      end
      | `slli -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) <<: (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `srli -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>: (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `srai -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>+ (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `addw -> begin
        riscv.regs.(rd i) <- sextd 31 D.(riscv.regs.(rs1 i) +: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `subw -> begin
        riscv.regs.(rd i) <- sextd 31 D.(riscv.regs.(rs1 i) -: riscv.regs.(rs2 i));
        incr_pc()
      end
      | `sllw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) <<: (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
        incr_pc()
      end
      | `srlw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>: (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
        incr_pc()
      end
      | `sraw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>+ (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
        incr_pc()
      end
      | _ -> raise RISCV_illegal_instruction
  end

  module RV64M = struct
    let exec riscv i opcode = 
      match opcode with
      | `mulw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `divw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `divuw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `remw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `remuw -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction
  end

  module RV64A = struct
    let exec riscv i opcode = 
      match opcode with
      | `amoadd_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoxor_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoor_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoand_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amomin_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amomax_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amominu_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amomaxu_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `amoswap_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `lr_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `sc_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction
  end

  module RV64F = struct
    let exec riscv i opcode = 
      match opcode with
      | `fcvt_l_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_lu_s -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_s_l -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_s_lu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction
  end

  module RV64D = struct
    let exec riscv i opcode = 
      match opcode with
      | `fcvt_l_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_lu_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmv_x_d -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_d_l -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fcvt_d_lu -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | `fmv_d_x -> begin
        raise RISCV_instruction_not_yet_implemented
      end
      | _ -> raise RISCV_illegal_instruction
  end

  module RV32G = struct
    let exec riscv i opcode = 
      let rec f = function
        | [] -> raise RISCV_illegal_instruction
        | h::t -> begin
          try h riscv i opcode 
          with RISCV_illegal_instruction -> f t
        end
      in
      f [
        RV32I.exec;
        RV32M.exec;
        RV32A.exec;
        RV32F.exec;
        RV32D.exec;
        RVSYS.exec;
      ]
  end

  module RV64G = struct
    let exec riscv i opcode = 
      let rec f = function
        | [] -> raise RISCV_illegal_instruction
        | h::t -> begin
          try h riscv i opcode 
          with RISCV_illegal_instruction -> f t
        end
      in
      f [
        RV32G.exec;
        RV64I.exec;
        RV64M.exec;
        RV64A.exec;
        RV64F.exec;
        RV64D.exec;
      ]
  end

end

