module T = struct

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

let mask_match = [
  `c_mv    , (0x0000f003l,0x00000000l);
  `c_add   , (0x0000f003l,0x00001000l);
  `c_fsd   , (0x0000e003l,0x00002000l);
  `c_sw    , (0x0000e003l,0x00004000l);
  `c_fsw   , (0x0000e003l,0x00006000l);
  `c_addi4spn, (0x0000e003l,0x00008000l);
  `c_fld   , (0x0000e003l,0x0000a000l);
  `c_lw    , (0x0000e003l,0x0000c000l);
  `c_flw   , (0x0000e003l,0x0000e000l);
  `c_slli  , (0x0000e003l,0x00000001l);
  `c_fsdsp , (0x0000e003l,0x00002001l);
  `c_swsp  , (0x0000e003l,0x00004001l);
  `c_fswsp , (0x0000e003l,0x00006001l);
  `c_addw  , (0x0000f003l,0x00008001l);
  `c_fldsp , (0x0000e003l,0x0000a001l);
  `c_lwsp  , (0x0000e003l,0x0000c001l);
  `c_flwsp , (0x0000e003l,0x0000e001l);
  `c_j     , (0x0000e003l,0x00000002l);
  `c_jal   , (0x0000e003l,0x00002002l);
  `c_beqz  , (0x0000e003l,0x00004002l);
  `c_bnez  , (0x0000e003l,0x00006002l);
  `c_li    , (0x0000e003l,0x00008002l);
  `c_lui   , (0x0000e003l,0x0000a002l);
  `c_addi  , (0x0000e003l,0x0000c002l);
  `c_addiw , (0x0000e003l,0x0000e002l);
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
  | `c_mv     ->
    ("c.mv")
  | `c_add    ->
    ("c.add")
  | `c_fsd    ->
    ("c.fsd")
  | `c_sw     ->
    ("c.sw")
  | `c_fsw    ->
    ("c.fsw")
  | `c_addi4spn ->
    ("c.addi4spn")
  | `c_fld    ->
    ("c.fld")
  | `c_lw     ->
    ("c.lw")
  | `c_flw    ->
    ("c.flw")
  | `c_slli   ->
    ("c.slli")
  | `c_fsdsp  ->
    ("c.fsdsp")
  | `c_swsp   ->
    ("c.swsp")
  | `c_fswsp  ->
    ("c.fswsp")
  | `c_addw   ->
    ("c.addw")
  | `c_fldsp  ->
    ("c.fldsp")
  | `c_lwsp   ->
    ("c.lwsp")
  | `c_flwsp  ->
    ("c.flwsp")
  | `c_j      ->
    ("c.j")
  | `c_jal    ->
    ("c.jal")
  | `c_beqz   ->
    ("c.beqz")
  | `c_bnez   ->
    ("c.bnez")
  | `c_li     ->
    ("c.li")
  | `c_lui    ->
    ("c.lui")
  | `c_addi   ->
    ("c.addi")
  | `c_addiw  ->
    ("c.addiw")
let fields =
  let open Types.Fields in
  [
    (`c_mv, [ Range((1,0),Int(0)); Range((15,13),Int(0)); Bit(12,Int(0)); Range((11,2),Ignore); ]);
    (`c_add, [ Range((1,0),Int(0)); Range((15,13),Int(0)); Bit(12,Int(1)); Range((11,2),Ignore); ]);
    (`c_fsd, [ Range((1,0),Int(0)); Range((15,13),Int(1)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_sw, [ Range((1,0),Int(0)); Range((15,13),Int(2)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_fsw, [ Range((1,0),Int(0)); Range((15,13),Int(3)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_addi4spn, [ Range((1,0),Int(0)); Range((15,13),Int(4)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_fld, [ Range((1,0),Int(0)); Range((15,13),Int(5)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_lw, [ Range((1,0),Int(0)); Range((15,13),Int(6)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_flw, [ Range((1,0),Int(0)); Range((15,13),Int(7)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_slli, [ Range((1,0),Int(1)); Range((15,13),Int(0)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_fsdsp, [ Range((1,0),Int(1)); Range((15,13),Int(1)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_swsp, [ Range((1,0),Int(1)); Range((15,13),Int(2)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_fswsp, [ Range((1,0),Int(1)); Range((15,13),Int(3)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_addw, [ Range((1,0),Int(1)); Range((15,13),Int(4)); Bit(12,Int(0)); Range((11,2),Ignore); ]);
    (`c_fldsp, [ Range((1,0),Int(1)); Range((15,13),Int(5)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_lwsp, [ Range((1,0),Int(1)); Range((15,13),Int(6)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_flwsp, [ Range((1,0),Int(1)); Range((15,13),Int(7)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_j, [ Range((1,0),Int(2)); Range((15,13),Int(0)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_jal, [ Range((1,0),Int(2)); Range((15,13),Int(1)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_beqz, [ Range((1,0),Int(2)); Range((15,13),Int(2)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_bnez, [ Range((1,0),Int(2)); Range((15,13),Int(3)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_li, [ Range((1,0),Int(2)); Range((15,13),Int(4)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_lui, [ Range((1,0),Int(2)); Range((15,13),Int(5)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_addi, [ Range((1,0),Int(2)); Range((15,13),Int(6)); Bit(12,Ignore); Range((11,2),Ignore); ]);
    (`c_addiw, [ Range((1,0),Int(2)); Range((15,13),Int(7)); Bit(12,Ignore); Range((11,2),Ignore); ]);
  ]

end

module Asm_raw = struct

let c_mv = Types.I.(
  0x0l)

let c_add = Types.I.(
  0x1000l)

let c_fsd = Types.I.(
  0x2000l)

let c_sw = Types.I.(
  0x4000l)

let c_fsw = Types.I.(
  0x6000l)

let c_addi4spn = Types.I.(
  0x8000l)

let c_fld = Types.I.(
  0xa000l)

let c_lw = Types.I.(
  0xc000l)

let c_flw = Types.I.(
  0xe000l)

let c_slli = Types.I.(
  0x1l)

let c_fsdsp = Types.I.(
  0x2001l)

let c_swsp = Types.I.(
  0x4001l)

let c_fswsp = Types.I.(
  0x6001l)

let c_addw = Types.I.(
  0x8001l)

let c_fldsp = Types.I.(
  0xa001l)

let c_lwsp = Types.I.(
  0xc001l)

let c_flwsp = Types.I.(
  0xe001l)

let c_j = Types.I.(
  0x2l)

let c_jal = Types.I.(
  0x2002l)

let c_beqz = Types.I.(
  0x4002l)

let c_bnez = Types.I.(
  0x6002l)

let c_li = Types.I.(
  0x8002l)

let c_lui = Types.I.(
  0xa002l)

let c_addi = Types.I.(
  0xc002l)

let c_addiw = Types.I.(
  0xe002l)

end

module Asm = struct

let c_mv = Asm_raw.c_mv
let c_add = Asm_raw.c_add
let c_fsd = Asm_raw.c_fsd
let c_sw = Asm_raw.c_sw
let c_fsw = Asm_raw.c_fsw
let c_addi4spn = Asm_raw.c_addi4spn
let c_fld = Asm_raw.c_fld
let c_lw = Asm_raw.c_lw
let c_flw = Asm_raw.c_flw
let c_slli = Asm_raw.c_slli
let c_fsdsp = Asm_raw.c_fsdsp
let c_swsp = Asm_raw.c_swsp
let c_fswsp = Asm_raw.c_fswsp
let c_addw = Asm_raw.c_addw
let c_fldsp = Asm_raw.c_fldsp
let c_lwsp = Asm_raw.c_lwsp
let c_flwsp = Asm_raw.c_flwsp
let c_j = Asm_raw.c_j
let c_jal = Asm_raw.c_jal
let c_beqz = Asm_raw.c_beqz
let c_bnez = Asm_raw.c_bnez
let c_li = Asm_raw.c_li
let c_lui = Asm_raw.c_lui
let c_addi = Asm_raw.c_addi
let c_addiw = Asm_raw.c_addiw
end

module Test = struct

let suite f n = [
]

end

