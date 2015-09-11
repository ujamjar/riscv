module T : sig

type t = [
| `sret
| `sfence_vm
| `wfi
| `mrth
| `mrts
| `hrts
| `csrrw
| `csrrs
| `csrrc
| `csrrwi
| `csrrsi
| `csrrci
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val sret : Types.I.t
val sfence_vm : rs1:int -> Types.I.t
val wfi : Types.I.t
val mrth : Types.I.t
val mrts : Types.I.t
val hrts : Types.I.t
val csrrw : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrs : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrc : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrwi : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrsi : rd:int -> rs1:int -> imm12:int -> Types.I.t
val csrrci : rd:int -> rs1:int -> imm12:int -> Types.I.t

end

module Asm : sig

val sret : Types.I.t
val sfence_vm : rs1:int -> Types.I.t
val wfi : Types.I.t
val mrth : Types.I.t
val mrts : Types.I.t
val hrts : Types.I.t
val csrrw : rd:int -> rs1:int -> imm:int -> Types.I.t
val csrrs : rd:int -> rs1:int -> imm:int -> Types.I.t
val csrrc : rd:int -> rs1:int -> imm:int -> Types.I.t
val csrrwi : rd:int -> rs1:int -> imm:int -> Types.I.t
val csrrsi : rd:int -> rs1:int -> imm:int -> Types.I.t
val csrrci : rd:int -> rs1:int -> imm:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

