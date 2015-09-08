module T : sig

  type t = [
    | RV32G.T.t
    | RV64I.T.t
    | RV64M.T.t
    | RV64A.T.t
    | RV64F.T.t
    | RV64D.T.t
  ]

  val mask_match : (t * (Int32.t * Int32.t)) list

  val to_t : Int32.t -> t

  val pretty : Int32.t -> string
  
  val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig
  include module type of RV32G.Asm_raw
  include module type of RV64I.Asm_raw
  include module type of RV64M.Asm_raw
  include module type of RV64A.Asm_raw
  include module type of RV64F.Asm_raw
  include module type of RV64D.Asm_raw
end

module Test : sig
  val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite
end

