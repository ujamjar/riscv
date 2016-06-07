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
        if I.(all &: (sll 1l pos)) = 0l then 
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
        let msk = I.( (sll 1l (h-l+1)) -: 1l ) in
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
          let msk = I.( (sll 1l (h-l+1)) -: 1l ) in
          (* create an array indexed by instr.[h..l] *)
          let partition = partition instrs (l,h) in
          let a = Array.init (1 lsl (h-l+1))
            (fun i -> 
              let instrs = try List.assoc Int32.(of_int i) partition with _ -> [] in
              f I.(done_mask |: (sll msk l)) instrs)
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
    D.of_i @@ I.((t &: 0xFFFFF7FEl) |: (sll (t &: one) 11))
  let u_imm i = D.of_i @@ I.(i &: 0xFFFFF000l)
  let j_imm i = 
    let t = I.((i >>+ 20) &: 0xFFF007FEl) in
    D.of_i @@ I.(t |: ((i >>: 9) &: 0x800l) |: (i &: 0xFF000l))
  let s_imm i = D.of_i @@ s_imm i

  let sexti pos v = 
    let pos = I.bits - pos - 1 in
    I.((sll v pos) >>+ pos)
  let sextd pos v = 
    let pos = D.bits - pos - 1 in
    D.((sll v pos) >>+ pos)

  let mask_dlbits = D.mask D.lbits
  let four = D.(sll one 2)

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
      | `slli -> begin
        riscv.regs.(rd i) <- D.(sll riscv.regs.(rs1 i) (to_int (i_imm i &: mask_dlbits)));
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
      | `srli -> begin
        riscv.regs.(rd i) <- D.(riscv.regs.(rs1 i) >>: (to_int (i_imm i &: mask_dlbits)));
        incr_pc()
      end
      | `srai -> begin
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
          D.(sll riscv.regs.(rs1 i) (to_int (riscv.regs.(rs2 i) &: mask_dlbits)));
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
      | `ecall -> begin
        incr_pc(); (* XXX I think... *)
        raise RISCV_system_call
      end
      | `ebreak -> begin
        raise RISCV_breakpoint (* what about PC? *)
      end
      | `csrrs -> raise RISCV_instruction_not_yet_implemented
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

  (*module RVSYS = struct

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

  end*)

  module RV64I = struct
    let exec riscv i opcode = 
      let incr_pc() = riscv.pc <- D.(riscv.pc +: four) in
      let mask_dlbits_1 = D.(mask_dlbits >>: 1) in
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
      | `slliw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(sll riscv.regs.(rs1 i) (to_int (i_imm i &: mask_dlbits_1)));
        incr_pc()
      end
      | `srliw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>: (to_int (i_imm i &: mask_dlbits_1)));
        incr_pc()
      end
      | `sraiw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>+ (to_int (i_imm i &: mask_dlbits_1)));
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
          D.(sll riscv.regs.(rs1 i) (to_int (riscv.regs.(rs2 i) &: mask_dlbits_1)));
        incr_pc()
      end
      | `srlw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>: (to_int (riscv.regs.(rs2 i) &: mask_dlbits_1)));
        incr_pc()
      end
      | `sraw -> begin
        riscv.regs.(rd i) <- sextd 31 
          D.(riscv.regs.(rs1 i) >>+ (to_int (riscv.regs.(rs2 i) &: mask_dlbits_1)));
        incr_pc()
      end
      | _ -> raise RISCV_illegal_instruction

    let exec riscv i opcode = 
      try RV32I.exec riscv i opcode 
      with RISCV_illegal_instruction -> exec riscv i opcode

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
        (*RVSYS.exec;*)
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
        RV64I.exec;
        RV64M.exec;
        RV64A.exec;
        RV64F.exec;
        RV64D.exec;
        RV32M.exec;
        RV32A.exec;
        RV32F.exec;
        RV32D.exec;
        (*RVSYS.exec;*)
      ]
  end

end

