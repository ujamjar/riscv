module T : sig

type t = [
| `mulw
| `divw
| `divuw
| `remw
| `remuw
] deriving(Enum,Bounded,Show)

val name : string

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val mulw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divuw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remuw : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

module Asm : sig

val mulw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val divuw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remw : rd:int -> rs1:int -> rs2:int -> Types.I.t
val remuw : rd:int -> rs1:int -> rs2:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

