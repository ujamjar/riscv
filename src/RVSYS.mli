module T : sig

type t = [
| `scall
| `sbreak
| `sret
| `csrrw
| `csrrs
| `csrrc
| `csrrwi
| `csrrsi
| `csrrci
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val scall : Types.I.t
val sbreak : Types.I.t
val sret : Types.I.t
val csrrw : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrs : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrc : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrwi : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrsi : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrci : rd:int -> rs1:int -> imm12:int -> Types.I.t

end

