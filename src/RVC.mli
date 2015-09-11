module T : sig

type t = [
| `c_mv
| `c_add
| `c_fsd
| `c_sw
| `c_fsw
| `c_addi4spn
| `c_fld
| `c_lw
| `c_flw
| `c_slli
| `c_fsdsp
| `c_swsp
| `c_fswsp
| `c_addw
| `c_fldsp
| `c_lwsp
| `c_flwsp
| `c_j
| `c_jal
| `c_beqz
| `c_bnez
| `c_li
| `c_lui
| `c_addi
| `c_addiw
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val c_mv : Types.I.t
val c_add : Types.I.t
val c_fsd : Types.I.t
val c_sw : Types.I.t
val c_fsw : Types.I.t
val c_addi4spn : Types.I.t
val c_fld : Types.I.t
val c_lw : Types.I.t
val c_flw : Types.I.t
val c_slli : Types.I.t
val c_fsdsp : Types.I.t
val c_swsp : Types.I.t
val c_fswsp : Types.I.t
val c_addw : Types.I.t
val c_fldsp : Types.I.t
val c_lwsp : Types.I.t
val c_flwsp : Types.I.t
val c_j : Types.I.t
val c_jal : Types.I.t
val c_beqz : Types.I.t
val c_bnez : Types.I.t
val c_li : Types.I.t
val c_lui : Types.I.t
val c_addi : Types.I.t
val c_addiw : Types.I.t

end

module Asm : sig

val c_mv : Types.I.t
val c_add : Types.I.t
val c_fsd : Types.I.t
val c_sw : Types.I.t
val c_fsw : Types.I.t
val c_addi4spn : Types.I.t
val c_fld : Types.I.t
val c_lw : Types.I.t
val c_flw : Types.I.t
val c_slli : Types.I.t
val c_fsdsp : Types.I.t
val c_swsp : Types.I.t
val c_fswsp : Types.I.t
val c_addw : Types.I.t
val c_fldsp : Types.I.t
val c_lwsp : Types.I.t
val c_flwsp : Types.I.t
val c_j : Types.I.t
val c_jal : Types.I.t
val c_beqz : Types.I.t
val c_bnez : Types.I.t
val c_li : Types.I.t
val c_lui : Types.I.t
val c_addi : Types.I.t
val c_addiw : Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

