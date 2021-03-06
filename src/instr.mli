exception RISCV_illegal_instruction
exception RISCV_floating_point_disabled
exception RISCV_instruction_not_yet_implemented
exception RISCV_system_call
exception RISCV_breakpoint

module Util : sig
  open Types
  type 'a t = ('a * (I.t * I.t)) list

  val constant_10 : 'a t -> I.t * I.t
  val constant_mask : 'a t -> I.t
  val constant_all : 'a t -> I.t
  
  val ranges : I.t -> (int * int) list
  val partition : 'a t -> (int * int) -> (I.t * 'a t) list
  val instruction_decoder : 'a t -> (I.t -> 'a)
  val instruction_decoder_simple : 'a t -> (I.t -> 'a)
end

module Make(T : Cpu.T) : sig
  module RV32I : sig
    val exec : T.riscv -> Types.I.t -> [> RV32I.T.t ] -> unit
  end
  module RV32M : sig
    val exec : T.riscv -> Types.I.t -> [> RV32M.T.t ] -> unit
  end
  module RV32A : sig
    val exec : T.riscv -> Types.I.t -> [> RV32A.T.t ] -> unit
  end
  module RV32F : sig
    val exec : T.riscv -> Types.I.t -> [> RV32F.T.t ] -> unit
  end
  module RV32D : sig
    val exec : T.riscv -> Types.I.t -> [> RV32D.T.t ] -> unit
  end
  (*module RVSYS : sig
    val exec : T.riscv -> Types.I.t -> [> RVSYS.T.t ] -> unit
  end*)
  module RV64I : sig
    val exec : T.riscv -> Types.I.t -> [> RV64I.T.t ] -> unit
  end
  module RV64M : sig
    val exec : T.riscv -> Types.I.t -> [> RV64M.T.t ] -> unit
  end
  module RV64A : sig
    val exec : T.riscv -> Types.I.t -> [> RV64A.T.t ] -> unit
  end
  module RV64F : sig
    val exec : T.riscv -> Types.I.t -> [> RV64F.T.t ] -> unit
  end
  module RV64D : sig
    val exec : T.riscv -> Types.I.t -> [> RV64D.T.t ] -> unit
  end
  module RV32G : sig
    val exec : T.riscv -> Types.I.t -> [> RV32G.T.t ] -> unit
  end
  module RV64G : sig
    val exec : T.riscv -> Types.I.t -> [> RV64G.T.t ] -> unit
  end
end

