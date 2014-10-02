module T = struct

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
]

let mask_match = [
  `fld     , (0x0000707fl,0x00003007l);
  `fsd     , (0x0000707fl,0x00003027l);
  `fadd_d  , (0xfe00007fl,0x02000053l);
  `fsub_d  , (0xfe00007fl,0x0a000053l);
  `fmul_d  , (0xfe00007fl,0x12000053l);
  `fdiv_d  , (0xfe00007fl,0x1a000053l);
  `fsgnj_d , (0xfe00707fl,0x22000053l);
  `fsgnjn_d, (0xfe00707fl,0x22001053l);
  `fsgnjx_d, (0xfe00707fl,0x22002053l);
  `fmin_d  , (0xfe00707fl,0x2a000053l);
  `fmax_d  , (0xfe00707fl,0x2a001053l);
  `fcvt_s_d, (0xfff0007fl,0x40100053l);
  `fcvt_d_s, (0xfff0007fl,0x42000053l);
  `fsqrt_d , (0xfff0007fl,0x5a000053l);
  `fle_d   , (0xfe00707fl,0xa2000053l);
  `flt_d   , (0xfe00707fl,0xa2001053l);
  `feq_d   , (0xfe00707fl,0xa2002053l);
  `fcvt_w_d, (0xfff0007fl,0xc2000053l);
  `fcvt_wu_d, (0xfff0007fl,0xc2100053l);
  `fclass_d, (0xfff0707fl,0xe2001053l);
  `fcvt_d_w, (0xfff0007fl,0xd2000053l);
  `fcvt_d_wu, (0xfff0007fl,0xd2100053l);
  `fmadd_d , (0x0600007fl,0x02000043l);
  `fmsub_d , (0x0600007fl,0x02000047l);
  `fnmsub_d, (0x0600007fl,0x0200004bl);
  `fnmadd_d, (0x0600007fl,0x0200004fl);
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
  | `fld      ->
    ("fld" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `fsd      ->
    ("fsd" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `fadd_d   ->
    ("fadd.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fsub_d   ->
    ("fsub.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fmul_d   ->
    ("fmul.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fdiv_d   ->
    ("fdiv.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rm=" ^ (x 14 12))
  | `fsgnj_d  ->
    ("fsgnj.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fsgnjn_d ->
    ("fsgnjn.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fsgnjx_d ->
    ("fsgnjx.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fmin_d   ->
    ("fmin.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fmax_d   ->
    ("fmax.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fcvt_s_d ->
    ("fcvt.s.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_d_s ->
    ("fcvt.d.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fsqrt_d  ->
    ("fsqrt.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fle_d    ->
    ("fle.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `flt_d    ->
    ("flt.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `feq_d    ->
    ("feq.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `fcvt_w_d ->
    ("fcvt.w.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_wu_d ->
    ("fcvt.wu.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fclass_d ->
    ("fclass.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `fcvt_d_w ->
    ("fcvt.d.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_d_wu ->
    ("fcvt.d.wu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fmadd_d  ->
    ("fmadd.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
  | `fmsub_d  ->
    ("fmsub.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
  | `fnmsub_d ->
    ("fnmsub.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
  | `fnmadd_d ->
    ("fnmadd.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " rs3=" ^ (x 31 27) ^ " rm=" ^ (x 14 12))
let fields =
  let open Types.Fields in
  [
    (`fld, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(1)); Range((1,0),Int(3)); ]);
    (`fsd, [ Field((`imm12hi,"imm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`imm12lo,"imm12lo",(11,7)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(9)); Range((1,0),Int(3)); ]);
    (`fadd_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(0)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fsub_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(1)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fmul_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(2)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fdiv_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(3)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fsgnj_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(4)); Range((14,12),Int(0)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fsgnjn_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(4)); Range((14,12),Int(1)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fsgnjx_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(4)); Range((14,12),Int(2)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fmin_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(5)); Range((14,12),Int(0)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fmax_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(5)); Range((14,12),Int(1)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_s_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(1)); Range((31,27),Int(8)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(0)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_d_s, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(8)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fsqrt_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(11)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fle_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(20)); Range((14,12),Int(0)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`flt_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(20)); Range((14,12),Int(1)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`feq_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,27),Int(20)); Range((14,12),Int(2)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_w_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(24)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_wu_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(1)); Range((31,27),Int(24)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fclass_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(28)); Range((14,12),Int(1)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_d_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(26)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_d_wu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(1)); Range((31,27),Int(26)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fmadd_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`rs3,"rs3",(31,27)), Nothing); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(16)); Range((1,0),Int(3)); ]);
    (`fmsub_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`rs3,"rs3",(31,27)), Nothing); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(17)); Range((1,0),Int(3)); ]);
    (`fnmsub_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`rs3,"rs3",(31,27)), Nothing); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(18)); Range((1,0),Int(3)); ]);
    (`fnmadd_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`rs3,"rs3",(31,27)), Nothing); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(19)); Range((1,0),Int(3)); ]);
  ]

end

module Asm = struct

let fld ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x3007l)

let fsd ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x3027l)

let fadd_d ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x2000053l)

let fsub_d ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xa000053l)

let fmul_d ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x12000053l)

let fdiv_d ~rd ~rs1 ~rs2 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x1a000053l)

let fsgnj_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x22000053l)

let fsgnjn_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x22001053l)

let fsgnjx_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x22002053l)

let fmin_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2a000053l)

let fmax_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2a001053l)

let fcvt_s_d ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x40100053l)

let fcvt_d_s ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x42000053l)

let fsqrt_d ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x5a000053l)

let fle_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0xa2000053l)

let flt_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0xa2001053l)

let feq_d ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0xa2002053l)

let fcvt_w_d ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc2000053l)

let fcvt_wu_d ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc2100053l)

let fclass_d ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0xe2001053l)

let fcvt_d_w ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd2000053l)

let fcvt_d_wu ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd2100053l)

let fmadd_d ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x2000043l)

let fmsub_d ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x2000047l)

let fnmsub_d ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x200004bl)

let fnmadd_d ~rd ~rs1 ~rs2 ~rs3 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int rs3) &: 0x1fl) <<: 27) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0x200004fl)

end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"fld" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `fld (Asm.fld ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"fsd" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (imm12hi, rs1, rs2, imm12lo) -> f `fsd (Asm.fsd ~imm12hi ~rs1 ~rs2 ~imm12lo)));
  QCheck.( mk_test ~name:"fadd.d" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rm) -> f `fadd_d (Asm.fadd_d ~rd ~rs1 ~rs2 ~rm)));
  QCheck.( mk_test ~name:"fsub.d" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rm) -> f `fsub_d (Asm.fsub_d ~rd ~rs1 ~rs2 ~rm)));
  QCheck.( mk_test ~name:"fmul.d" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rm) -> f `fmul_d (Asm.fmul_d ~rd ~rs1 ~rs2 ~rm)));
  QCheck.( mk_test ~name:"fdiv.d" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rm) -> f `fdiv_d (Asm.fdiv_d ~rd ~rs1 ~rs2 ~rm)));
  QCheck.( mk_test ~name:"fsgnj.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `fsgnj_d (Asm.fsgnj_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"fsgnjn.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `fsgnjn_d (Asm.fsgnjn_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"fsgnjx.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `fsgnjx_d (Asm.fsgnjx_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"fmin.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `fmin_d (Asm.fmin_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"fmax.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `fmax_d (Asm.fmax_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"fcvt.s.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_s_d (Asm.fcvt_s_d ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.d.s" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_d_s (Asm.fcvt_d_s ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fsqrt.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fsqrt_d (Asm.fsqrt_d ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fle.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `fle_d (Asm.fle_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"flt.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `flt_d (Asm.flt_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"feq.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `feq_d (Asm.feq_d ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"fcvt.w.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_w_d (Asm.fcvt_w_d ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.wu.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_wu_d (Asm.fcvt_wu_d ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fclass.d" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (rd, rs1) -> f `fclass_d (Asm.fclass_d ~rd ~rs1)));
  QCheck.( mk_test ~name:"fcvt.d.w" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_d_w (Asm.fcvt_d_w ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.d.wu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_d_wu (Asm.fcvt_d_wu ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fmadd.d" ~n 
    ~pp:PP.(QCRV.PP.tuple5 int int int int int) ~limit:2
    Arbitrary.(QCRV.tuple5 (int 32) (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rs3, rm) -> f `fmadd_d (Asm.fmadd_d ~rd ~rs1 ~rs2 ~rs3 ~rm)));
  QCheck.( mk_test ~name:"fmsub.d" ~n 
    ~pp:PP.(QCRV.PP.tuple5 int int int int int) ~limit:2
    Arbitrary.(QCRV.tuple5 (int 32) (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rs3, rm) -> f `fmsub_d (Asm.fmsub_d ~rd ~rs1 ~rs2 ~rs3 ~rm)));
  QCheck.( mk_test ~name:"fnmsub.d" ~n 
    ~pp:PP.(QCRV.PP.tuple5 int int int int int) ~limit:2
    Arbitrary.(QCRV.tuple5 (int 32) (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rs3, rm) -> f `fnmsub_d (Asm.fnmsub_d ~rd ~rs1 ~rs2 ~rs3 ~rm)));
  QCheck.( mk_test ~name:"fnmadd.d" ~n 
    ~pp:PP.(QCRV.PP.tuple5 int int int int int) ~limit:2
    Arbitrary.(QCRV.tuple5 (int 32) (int 32) (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rs2, rs3, rm) -> f `fnmadd_d (Asm.fnmadd_d ~rd ~rs1 ~rs2 ~rs3 ~rm)));
]

end

