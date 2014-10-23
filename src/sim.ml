(* RV64G simulation.  To be made generic over instruction set (if possible) *)

module D = Types.D64
module T = Types.Make(D)
module R = Instr.Make(T)
module M = Mem.Make(T)
module E = Load_elf.Make(T)

let init ~mem_size_mb ~elf_file ~pc = 
  let riscv = T.riscv_init mem_size_mb in
  let _ = E.to_mem elf_file riscv.T.mem in
  riscv.T.pc <- D.of_int pc;
  riscv

let decode = Instr.Util.instruction_decoder RV64G.T.mask_match 

let instr riscv = 
  Int64.to_int32 (M.load 2 riscv.T.mem (D.to_int riscv.T.pc)) 

let step riscv = 
  (* load instruction *)
  let i = instr riscv in
  (* execute instruction *)
  let () = R.RV64G.exec riscv i (decode i) in
  (* force r0 to zero *)
  let () = riscv.T.regs.(0) <- D.zero in
  ()

let pstep riscv = 
  let open Printf in
  let () = printf "%s\n%!" (RV64G.T.pretty (instr riscv)) in
  let () = step riscv in
  let () = printf "PC=%Li\n%!" riscv.T.pc  in
  let () = 
    for i=0 to 7 do
      printf "| ";
      for j=0 to 3 do
        let k = i * 4 + j in
        printf "%2x: %16Lx | " k riscv.T.regs.(k)
      done;
      printf "\n%!"
    done
  in
  ()