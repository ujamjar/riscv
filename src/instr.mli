exception RISCV_illegal_instruction
exception RISCV_floating_point_disabled
exception RISCV_instruction_not_yet_implemented
exception RISCV_system_call
exception RISCV_breakpoint

module Make(T : Types.T) : sig

  val i_type : T.riscv -> T.instr -> unit
  val r_type : T.riscv -> T.instr -> unit
  val lui : T.riscv -> T.instr -> unit
  val auipc : T.riscv -> T.instr -> unit
  val jal : T.riscv -> T.instr -> unit
  val b_type : T.riscv -> T.instr -> unit
  val jalr : T.riscv -> T.instr -> unit
  val loads : T.riscv -> T.instr -> unit
  val stores : T.riscv -> T.instr -> unit
  val fence : T.riscv -> T.instr -> unit
  val csr : T.riscv -> T.instr -> unit
  val sys : T.riscv -> T.instr -> unit
  val int32 : T.riscv -> T.instr -> unit
  val int32_more : T.riscv -> T.instr -> unit
  val atomic : T.riscv -> T.instr -> unit

  val execute : T.riscv -> T.instr -> unit

end
