module type RV = sig
  module D : Types.D
  module T : module type of Cpu.Make(D)
  module R : module type of Instr.Make(T)
  module M : module type of Mem.Make(T)
end

(*module T : Cpu.T with type D.t = int64*)
(*module T : Cpu.T with type D.t = int32*)
module RV : RV

val init : mem_size_mb:int -> elf_file:string -> pc:int -> RV.T.riscv
val instr : RV.T.riscv -> Types.I.t
val step : RV.T.riscv -> unit
val pstep : RV.T.riscv -> unit

