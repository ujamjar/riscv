
module T = Cpu.Make(Types.D64)
module Elf = Load_elf.Make(T)
module Mem = Mem.Make(T)

let riscv = T.riscv_init 1
let syms = Elf.to_mem "test/pk" riscv.T.mem

(*let _ = 
  Elf.String_map.iter 
    (fun k v -> let open Load_elf in Printf.printf "%s\n" k) syms*)

let () = 
  for i=0 to 31 do
    let instr = Mem.load 2 riscv.T.mem (0x2000+(i*4)) in
    Printf.printf "%s\n" (RV64G.T.pretty (Int64.to_int32 instr))
  done

