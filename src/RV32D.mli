module T : sig

type t = [
| `fld
| `fsd
| `fadd_d
| `fsub_d
| `fmul_d
| `fdiv_d
| `fsgnj_d
| `fsgnjn_d
| `fsgnjx_d
| `fmin_d
| `fmax_d
| `fcvt_s_d
| `fcvt_d_s
| `fsqrt_d
| `fle_d
| `flt_d
| `feq_d
| `fcvt_w_d
| `fcvt_wu_d
| `fclass_d
| `fcvt_d_w
| `fcvt_d_wu
| `fmadd_d
| `fmsub_d
| `fnmsub_d
| `fnmadd_d
] deriving(Enum,Bounded,Show)

val name : string

val mask_match : (t * (Types.I.t * Types.I.t)) list

val to_t : Types.I.t -> t

val pretty : Types.I.t -> string

val fields : (t * Types.Fields.t list) list

end

module Asm_raw : sig

val fld : rd:int -> rs1:int -> imm12:int -> Types.I.t
val fsd : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val fadd_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fsub_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fmul_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fdiv_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fsgnj_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsgnjn_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsgnjx_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fmin_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fmax_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fcvt_s_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_d_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fsqrt_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fle_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val flt_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val feq_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fcvt_w_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_wu_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fclass_d : rd:int -> rs1:int -> Types.I.t
val fcvt_d_w : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_d_wu : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmadd_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fmsub_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fnmsub_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fnmadd_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t

end

module Asm : sig

val fld : rd:int -> rs1:int -> imm:int -> Types.I.t
val fsd : rs1:int -> rs2:int -> imm:int -> Types.I.t
val fadd_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fsub_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fmul_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fdiv_d : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fsgnj_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsgnjn_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsgnjx_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fmin_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fmax_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fcvt_s_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_d_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fsqrt_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fle_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val flt_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val feq_d : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fcvt_w_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_wu_d : rd:int -> rs1:int -> rm:int -> Types.I.t
val fclass_d : rd:int -> rs1:int -> Types.I.t
val fcvt_d_w : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_d_wu : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmadd_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fmsub_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fnmsub_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fnmadd_d : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t

end

module Test : sig

val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite

end

