module T = struct

type t = [
| `lwu
| `ld
| `sd
| `slli
| `srli
| `srai
| `addiw
| `addw
| `subw
| `sllw
| `srlw
| `sraw
]

let mask_match = [
  `lwu     , (0x0000707fl,0x00006003l);
  `ld      , (0x0000707fl,0x00003003l);
  `sd      , (0x0000707fl,0x00003023l);
  `slli    , (0xfc00707fl,0x00001013l);
  `srli    , (0xfc00707fl,0x00005013l);
  `srai    , (0xfc00707fl,0x40005013l);
  `addiw   , (0x0000707fl,0x0000001bl);
  `addw    , (0xfe00707fl,0x0000003bl);
  `subw    , (0xfe00707fl,0x4000003bl);
  `sllw    , (0xfe00707fl,0x0000103bl);
  `srlw    , (0xfe00707fl,0x0000503bl);
  `sraw    , (0xfe00707fl,0x4000503bl);
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
  | `lwu      ->
    ("lwu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `ld       ->
    ("ld" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `sd       ->
    ("sd" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `slli     ->
    ("slli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `srli     ->
    ("srli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `srai     ->
    ("srai" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `addiw    ->
    ("addiw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `addw     ->
    ("addw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `subw     ->
    ("subw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sllw     ->
    ("sllw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `srlw     ->
    ("srlw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sraw     ->
    ("sraw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
let fields =
  let open Types.Fields in
  [
    (`lwu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`ld, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`sd, [ Field((`imm12hi,"imm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`imm12lo,"imm12lo",(11,7)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(8)); Range((1,0),Int(3)); ]);
    (`slli, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,26),Int(0)); Field((`shamt,"shamt",(25,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`srli, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,26),Int(0)); Field((`shamt,"shamt",(25,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`srai, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,26),Int(16)); Field((`shamt,"shamt",(25,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`addiw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(0)); Range((6,2),Int(6)); Range((1,0),Int(3)); ]);
    (`addw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(0)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`subw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(32)); Range((14,12),Int(0)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`sllw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(1)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`srlw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(5)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`sraw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(32)); Range((14,12),Int(5)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let lwu ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x6003l)

let ld ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x3003l)

let sd ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x3023l)

let slli ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x1013l)

let srli ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x5013l)

let srai ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x40005013l)

let addiw ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x1bl)

let addw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x3bl)

let subw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x4000003bl)

let sllw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x103bl)

let srlw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x503bl)

let sraw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x4000503bl)

end

module Asm = struct

let lwu ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.lwu ~rd ~rs1 ) ~imm
let ld ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.ld ~rd ~rs1 ) ~imm
let sd ~rs1 ~rs2 ~imm = Imm.s_imm (Asm_raw.sd ~rs1 ~rs2 ) ~imm
let slli ~rd ~rs1 ~imm = Imm.sh_imm (Asm_raw.slli ~rd ~rs1 ) ~imm
let srli ~rd ~rs1 ~imm = Imm.sh_imm (Asm_raw.srli ~rd ~rs1 ) ~imm
let srai ~rd ~rs1 ~imm = Imm.sh_imm (Asm_raw.srai ~rd ~rs1 ) ~imm
let addiw ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.addiw ~rd ~rs1 ) ~imm
let addw = Asm_raw.addw
let subw = Asm_raw.subw
let sllw = Asm_raw.sllw
let srlw = Asm_raw.srlw
let sraw = Asm_raw.sraw
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"lwu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `lwu (Asm_raw.lwu ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"ld" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `ld (Asm_raw.ld ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"sd" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (imm12hi, rs1, rs2, imm12lo) -> f `sd (Asm_raw.sd ~imm12hi ~rs1 ~rs2 ~imm12lo)));
  QCheck.( mk_test ~name:"slli" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 64)) 
    (fun (rd, rs1, shamt) -> f `slli (Asm_raw.slli ~rd ~rs1 ~shamt)));
  QCheck.( mk_test ~name:"srli" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 64)) 
    (fun (rd, rs1, shamt) -> f `srli (Asm_raw.srli ~rd ~rs1 ~shamt)));
  QCheck.( mk_test ~name:"srai" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 64)) 
    (fun (rd, rs1, shamt) -> f `srai (Asm_raw.srai ~rd ~rs1 ~shamt)));
  QCheck.( mk_test ~name:"addiw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `addiw (Asm_raw.addiw ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"addw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `addw (Asm_raw.addw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"subw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `subw (Asm_raw.subw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"sllw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `sllw (Asm_raw.sllw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"srlw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `srlw (Asm_raw.srlw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"sraw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `sraw (Asm_raw.sraw ~rd ~rs1 ~rs2)));
]

end

