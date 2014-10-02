module T : sig

  type t = [
    | RV32I.T.t
    | RV32M.T.t
    | RV32A.T.t
    | RV32F.T.t
    | RV32D.T.t
  ]

  val mask_match : (t * (Int32.t * Int32.t)) list

  val to_t : Int32.t -> t

  val pretty : Int32.t -> string
  
  val fields : (t * Types.Fields.t list) list

end

module Asm : sig
  include module type of RV32I.Asm
  include module type of RV32M.Asm
  include module type of RV32A.Asm
  include module type of RV32F.Asm
  include module type of RV32D.Asm
end

module Test : sig
  val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite
end
