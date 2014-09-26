exception RISCV_illegal_instruction
exception RISCV_floating_point_disabled
exception RISCV_instruction_not_yet_implemented
exception RISCV_system_call
exception RISCV_breakpoint

val i_type : Types.riscv -> Types.instr -> unit
val r_type : Types.riscv -> Types.instr -> unit
val lui : Types.riscv -> Types.instr -> unit
val auipc : Types.riscv -> Types.instr -> unit
val jal : Types.riscv -> Types.instr -> unit
val b_type : Types.riscv -> Types.instr -> unit
val jalr : Types.riscv -> Types.instr -> unit
val loads : Types.riscv -> Types.instr -> unit
val stores : Types.riscv -> Types.instr -> unit
val fence : Types.riscv -> Types.instr -> unit
val csr : Types.riscv -> Types.instr -> unit
val sys : Types.riscv -> Types.instr -> unit
val int32 : Types.riscv -> Types.instr -> unit
val int32_more : Types.riscv -> Types.instr -> unit
val atomic : Types.riscv -> Types.instr -> unit

val execute : Types.riscv -> Types.instr -> unit

