module T : sig

  type t = [
    | RV64I.T.t
    | RV64M.T.t
    | RV64A.T.t
    | RV64F.T.t
    | RV64D.T.t
    | RV32M.T.t
    | RV32A.T.t
    | RV32F.T.t
    | RV32D.T.t
    (*| RVSYS.T.t*)
  ]

  val mask_match : (t * (Int32.t * Int32.t)) list

  val to_t : Int32.t -> t

  val pretty : Int32.t -> string
  
  val fields : (t * Types.Fields.t list) list

end

module Asm : sig
  include module type of RV64I.Asm
  include module type of RV64M.Asm
  include module type of RV64A.Asm
  include module type of RV64F.Asm
  include module type of RV64D.Asm
  include module type of RV32M.Asm
  include module type of RV32A.Asm
  include module type of RV32F.Asm
  include module type of RV32D.Asm
  (*include module type of RVSYS.Asm*)
end

module Test : sig
  val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite
end

