module T : sig

type t = [
| `_slli_rv32
| `_srli_rv32
| `_srai_rv32
| `_frflags
| `_fsflags
| `_fsflagsi
| `_frrm
| `_fsrm
| `_fsrmi
| `_fscsr
| `_frcsr
| `_rdcycle
| `_rdtime
| `_rdinstret
| `_rdcycleh
| `_rdtimeh
| `_rdinstreth
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val _slli_rv32 : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val _srli_rv32 : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val _srai_rv32 : rd:int -> rs1:int -> shamtw:int -> Types.I.t
val _frflags : rd:int -> Types.I.t
val _fsflags : rd:int -> rs1:int -> Types.I.t
val _fsflagsi : rd:int -> zimm:int -> Types.I.t
val _frrm : rd:int -> Types.I.t
val _fsrm : rd:int -> rs1:int -> Types.I.t
val _fsrmi : rd:int -> zimm:int -> Types.I.t
val _fscsr : rd:int -> rs1:int -> Types.I.t
val _frcsr : rd:int -> Types.I.t
val _rdcycle : rd:int -> Types.I.t
val _rdtime : rd:int -> Types.I.t
val _rdinstret : rd:int -> Types.I.t
val _rdcycleh : rd:int -> Types.I.t
val _rdtimeh : rd:int -> Types.I.t
val _rdinstreth : rd:int -> Types.I.t

end

