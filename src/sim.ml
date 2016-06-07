(* RV64G simulation.  To be made generic over instruction set (if possible) *)

module type RV = sig
  module D : Types.D
  module T : module type of Cpu.Make(D)
  module R : module type of Instr.Make(T)
  module M : module type of Mem.Make(T)
end


module RV64I = struct
  module D = Types.D64
  module T = Cpu.Make(D)
  module R = Instr.Make(T)
  module M = Mem.Make(T)
  module E = Load_elf.Make(T)
  module RRV = R.RV64I
  module RV = RV64I
end

module RV64G = struct
  module D = Types.D64
  module T = Cpu.Make(D)
  module R = Instr.Make(T)
  module M = Mem.Make(T)
  module E = Load_elf.Make(T)
  module RRV = R.RV64G
  module RV = RV64G
end

module RV32I = struct
  module D = Types.D32
  module T = Cpu.Make(D)
  module R = Instr.Make(T)
  module M = Mem.Make(T)
  module E = Load_elf.Make(T)
  module RRV = R.RV32I
  module RV = RV32I
end

module RV32G = struct
  module D = Types.D32
  module T = Cpu.Make(D)
  module R = Instr.Make(T)
  module M = Mem.Make(T)
  module E = Load_elf.Make(T)
  module RRV = R.RV32G
  module RV = RV32G
end

module RV = RV32I
open RV

let init ~mem_size_mb ~elf_file ~pc = 
  let riscv = T.riscv_init mem_size_mb in
  let _ = E.to_mem elf_file riscv.T.mem in
  riscv.T.pc <- D.of_int pc;
  riscv

let decode = Instr.Util.instruction_decoder RV.T.mask_match 

let instr riscv = 
  D.to_i (M.load 2 riscv.T.mem (D.to_int riscv.T.pc)) 

let step riscv = 
  (* load instruction *)
  let i = instr riscv in
  (* execute instruction *)
  let () = RRV.exec riscv i (decode i) in
  (* force r0 to zero *)
  let () = riscv.T.regs.(0) <- D.zero in
  ()

let pstep riscv = 
  let open Printf in
  let () = printf "%s\n%!" (RV.T.pretty (instr riscv)) in
  let () = step riscv in
  let () = printf "PC=%s\n%!" (D.to_string riscv.T.pc) in
  let () = 
    for i=0 to 7 do
      printf "| ";
      for j=0 to 3 do
        let k = i * 4 + j in
        printf "%2x: %s | " k (D.to_hex riscv.T.regs.(k))
      done;
      printf "\n%!"
    done
  in
  ()


