module T = struct

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

let mask_match = [
  `flw     , (0x0000707fl,0x00002007l);
  `fsw     , (0x0000707fl,0x00002027l);
  `fadd_s  , (0xfe00007fl,0x00000053l);
  `fsub_s  , (0xfe00007fl,0x08000053l);
  `fmul_s  , (0xfe00007fl,0x10000053l);
  `fdiv_s  , (0xfe00007fl,0x18000053l);
  `fsgnj_s , (0xfe00707fl,0x20000053l);
  `fsgnjn_s, (0xfe00707fl,0x20001053l);
  `fsgnjx_s, (0xfe00707fl,0x20002053l);
  `fmin_s  , (0xfe00707fl,0x28000053l);
  `fmax_s  , (0xfe00707fl,0x28001053l);
  `fsqrt_s , (0xfff0007fl,0x58000053l);
  `fle_s   , (0xfe00707fl,0xa0000053l);
  `flt_s   , (0xfe00707fl,0xa0001053l);
  `feq_s   , (0xfe00707fl,0xa0002053l);
  `fcvt_w_s, (0xfff0007fl,0xc0000053l);
  `fcvt_wu_s, (0xfff0007fl,0xc0100053l);
  `fmv_x_s , (0xfff0707fl,0xe0000053l);
  `fclass_s, (0xfff0707fl,0xe0001053l);
  `fcvt_s_w, (0xfff0007fl,0xd0000053l);
  `fcvt_s_wu, (0xfff0007fl,0xd0100053l);
  `fmv_s_x , (0xfff0707fl,0xf0000053l);
  `fmadd_s , (0x0600007fl,0x00000043l);
  `fmsub_s , (0x0600007fl,0x00000047l);
  `fnmsub_s, (0x0600007fl,0x0000004bl);
  `fnmadd_s, (0x0600007fl,0x0000004fl);
]

let to_t i = 
  let rec f = function
    | [] -> raise Not_found
    | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
  in
  f mask_match

let pretty i =
  let x h l = 
    Printf.sprintf "0x%lx" 
      Int32.(logand (shift_right i l) (sub (shift_left 1l (h-l+1)) 1l)) 
  in
  match to_t i with
  | `flw      ->
    ("flw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `fsw      ->
    ("fsw" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `fadd_s   ->
    ("fadd.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fsub_s   ->
    ("fsub.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fmul_s   ->
    ("fmul.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fdiv_s   ->
    ("fdiv.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fsgnj_s  ->
    ("fsgnj.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fsgnjn_s ->
    ("fsgnjn.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fsgnjx_s ->
    ("fsgnjx.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fmin_s   ->
    ("fmin.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fmax_s   ->
    ("fmax.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fsqrt_s  ->
    ("fsqrt.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fle_s    ->
    ("fle.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `flt_s    ->
    ("flt.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `feq_s    ->
    ("feq.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fcvt_w_s ->
    ("fcvt.w.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_wu_s ->
    ("fcvt.wu.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fmv_x_s  ->
    ("fmv.x.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `fclass_s ->
    ("fclass.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `fcvt_s_w ->
    ("fcvt.s.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_s_wu ->
    ("fcvt.s.wu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fmv_s_x  ->
    ("fmv.s.x" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `fmadd_s  ->
    ("fmadd.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
  | `fmsub_s  ->
    ("fmsub.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
  | `fnmsub_s ->
    ("fnmsub.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
  | `fnmadd_s ->
    ("fnmadd.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
end

module Asm = struct

let flw ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x2007l)

let fsw ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x2027l)

let fadd_s ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x53l)

let fsub_s ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x8000053l)

let fmul_s ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x10000053l)

let fdiv_s ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x18000053l)

let fsgnj_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x20000053l)

let fsgnjn_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x20001053l)

let fsgnjx_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x20002053l)

let fmin_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x28000053l)

let fmax_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x28001053l)

let fsqrt_s ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x58000053l)

let fle_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0xa0000053l)

let flt_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0xa0001053l)

let feq_s ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0xa0002053l)

let fcvt_w_s ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc0000053l)

let fcvt_wu_s ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc0100053l)

let fmv_x_s ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0xe0000053l)

let fclass_s ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0xe0001053l)

let fcvt_s_w ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd0000053l)

let fcvt_s_wu ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd0100053l)

let fmv_s_x ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0xf0000053l)

let fmadd_s ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x43l)

let fmsub_s ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x47l)

let fnmsub_s ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x4bl)

let fnmadd_s ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x4fl)

end

