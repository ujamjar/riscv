module T : Cpu.T with type D.t = int64

val init : mem_size_mb:int -> elf_file:string -> pc:int -> T.riscv
val instr : T.riscv -> Types.I.t
val step : T.riscv -> unit
val pstep : T.riscv -> unit

