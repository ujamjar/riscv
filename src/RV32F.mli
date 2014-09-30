module T : sig

type t = [
| `flw
| `fsw
| `fadd_s
| `fsub_s
| `fmul_s
| `fdiv_s
| `fsgnj_s
| `fsgnjn_s
| `fsgnjx_s
| `fmin_s
| `fmax_s
| `fsqrt_s
| `fle_s
| `flt_s
| `feq_s
| `fcvt_w_s
| `fcvt_wu_s
| `fmv_x_s
| `fclass_s
| `fcvt_s_w
| `fcvt_s_wu
| `fmv_s_x
| `fmadd_s
| `fmsub_s
| `fnmsub_s
| `fnmadd_s
]

val mask_match : (t * (Int32.t * Int32.t)) list

val to_t : Int32.t -> t

val pretty : Int32.t -> string

end

module Asm : sig

val flw : rd:int -> rs1:int -> imm12:int -> Types.I.t
val fsw : imm12hi:int -> rs1:int -> rs2:int -> imm12lo:int -> Types.I.t
val fadd_s : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fsub_s : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fmul_s : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fdiv_s : rd:int -> rs1:int -> rs2:int -> rm:int -> Types.I.t
val fsgnj_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsgnjn_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsgnjx_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fmin_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fmax_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fsqrt_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fle_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val flt_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val feq_s : rd:int -> rs1:int -> rs2:int -> Types.I.t
val fcvt_w_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_wu_s : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmv_x_s : rd:int -> rs1:int -> Types.I.t
val fclass_s : rd:int -> rs1:int -> Types.I.t
val fcvt_s_w : rd:int -> rs1:int -> rm:int -> Types.I.t
val fcvt_s_wu : rd:int -> rs1:int -> rm:int -> Types.I.t
val fmv_s_x : rd:int -> rs1:int -> Types.I.t
val fmadd_s : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fmsub_s : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fnmsub_s : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t
val fnmadd_s : rd:int -> rs1:int -> rs2:int -> rs3:int -> rm:int -> Types.I.t

end

