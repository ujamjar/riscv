module T : sig

type t = [
| `sfence_vm
] deriving(Enum,Bounded,Show)

val name : string

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val sfence_vm : rs1:int -> Types.I.t

end

module Asm : sig

val sfence_vm : rs1:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

