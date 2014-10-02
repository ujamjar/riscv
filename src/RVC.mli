module T : sig

type t = [
| `c_li
| `c_addi
| `c_addiw
| `c_ldsp
| `c_lwsp
| `c_sdsp
| `c_swsp
| `c_lw0
| `c_ld0
| `c_add
| `c_sub
| `c_move
| `c_j
| `c_ld
| `c_lw
| `c_sd
| `c_sw
| `c_beq
| `c_bne
| `c_flw
| `c_fld
| `c_fsw
| `c_fsd
| `c_slli
| `c_slli32
| `c_srli
| `c_srli32
| `c_srai
| `c_srai32
| `c_slliw
| `c_add3
| `c_sub3
| `c_or3
| `c_and3
]

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm : sig

val c_li : cimm6:int -> crd:int -> Types.I.t
val c_addi : cimm6:int -> crd:int -> Types.I.t
val c_addiw : cimm6:int -> crd:int -> Types.I.t
val c_ldsp : cimm6:int -> crd:int -> Types.I.t
val c_lwsp : cimm6:int -> crd:int -> Types.I.t
val c_sdsp : cimm6:int -> crd:int -> Types.I.t
val c_swsp : cimm6:int -> crd:int -> Types.I.t
val c_lw0 : crs1:int -> crd:int -> Types.I.t
val c_ld0 : crs1:int -> crd:int -> Types.I.t
val c_add : crs1:int -> crd:int -> Types.I.t
val c_sub : crs1:int -> crd:int -> Types.I.t
val c_move : crs1:int -> crd:int -> Types.I.t
val c_j : cimm10:int -> Types.I.t
val c_ld : crds:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_lw : crds:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_sd : crs2s:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_sw : crs2s:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_beq : crs2s:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_bne : crs2s:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_flw : crds:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_fld : crds:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_fsw : crs2s:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_fsd : crs2s:int -> crs1s:int -> cimm5:int -> Types.I.t
val c_slli : crds:int -> cimm5:int -> Types.I.t
val c_slli32 : crds:int -> cimm5:int -> Types.I.t
val c_srli : crds:int -> cimm5:int -> Types.I.t
val c_srli32 : crds:int -> cimm5:int -> Types.I.t
val c_srai : crds:int -> cimm5:int -> Types.I.t
val c_srai32 : crds:int -> cimm5:int -> Types.I.t
val c_slliw : crds:int -> cimm5:int -> Types.I.t
val c_add3 : crds:int -> crs1s:int -> crs2bs:int -> Types.I.t
val c_sub3 : crds:int -> crs1s:int -> crs2bs:int -> Types.I.t
val c_or3 : crds:int -> crs1s:int -> crs2bs:int -> Types.I.t
val c_and3 : crds:int -> crs1s:int -> crs2bs:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

