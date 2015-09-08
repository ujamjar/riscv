module T = struct

type t = [
| `amoadd_w
| `amoxor_w
| `amoor_w
| `amoand_w
| `amomin_w
| `amomax_w
| `amominu_w
| `amomaxu_w
| `amoswap_w
| `lr_w
| `sc_w
]

let mask_match = [
  `amoadd_w, (0xf800707fl,0x0000202fl);
  `amoxor_w, (0xf800707fl,0x2000202fl);
  `amoor_w , (0xf800707fl,0x4000202fl);
  `amoand_w, (0xf800707fl,0x6000202fl);
  `amomin_w, (0xf800707fl,0x8000202fl);
  `amomax_w, (0xf800707fl,0xa000202fl);
  `amominu_w, (0xf800707fl,0xc000202fl);
  `amomaxu_w, (0xf800707fl,0xe000202fl);
  `amoswap_w, (0xf800707fl,0x0800202fl);
  `lr_w    , (0xf9f0707fl,0x1000202fl);
  `sc_w    , (0xf800707fl,0x1800202fl);
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
  | `amoadd_w ->
    ("amoadd.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoxor_w ->
    ("amoxor.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoor_w  ->
    ("amoor.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoand_w ->
    ("amoand.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amomin_w ->
    ("amomin.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amomax_w ->
    ("amomax.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amominu_w ->
    ("amominu.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amomaxu_w ->
    ("amomaxu.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoswap_w ->
    ("amoswap.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `lr_w     ->
    ("lr.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " aqrl=" ^ (x 26 25))
  | `sc_w     ->
    ("sc.w" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
let fields =
  let open Types.Fields in
  [
    (`amoadd_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(0)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amoxor_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(1)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amoor_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(2)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amoand_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(3)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amomin_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(4)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amomax_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(5)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amominu_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(6)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amomaxu_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(7)); Range((28,27),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`amoswap_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(0)); Range((28,27),Int(1)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`lr_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(0)); Range((28,27),Int(2)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
    (`sc_w, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`aqrl,"aqrl",(26,25)), Nothing); Range((31,29),Int(0)); Range((28,27),Int(3)); Range((14,12),Int(2)); Range((6,2),Int(11)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let amoadd_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x202fl)

let amoxor_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x2000202fl)

let amoor_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x4000202fl)

let amoand_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x6000202fl)

let amomin_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x8000202fl)

let amomax_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0xa000202fl)

let amominu_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0xc000202fl)

let amomaxu_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0xe000202fl)

let amoswap_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x800202fl)

let lr_w ~rd ~rs1 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x1000202fl)

let sc_w ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x1800202fl)

end

module Asm = struct

let amoadd_w = Asm_raw.amoadd_w
let amoxor_w = Asm_raw.amoxor_w
let amoor_w = Asm_raw.amoor_w
let amoand_w = Asm_raw.amoand_w
let amomin_w = Asm_raw.amomin_w
let amomax_w = Asm_raw.amomax_w
let amominu_w = Asm_raw.amominu_w
let amomaxu_w = Asm_raw.amomaxu_w
let amoswap_w = Asm_raw.amoswap_w
let lr_w = Asm_raw.lr_w
let sc_w = Asm_raw.sc_w
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"amoadd.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amoadd_w (Asm_raw.amoadd_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amoxor.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amoxor_w (Asm_raw.amoxor_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amoor.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amoor_w (Asm_raw.amoor_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amoand.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amoand_w (Asm_raw.amoand_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amomin.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amomin_w (Asm_raw.amomin_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amomax.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amomax_w (Asm_raw.amomax_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amominu.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amominu_w (Asm_raw.amominu_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amomaxu.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amomaxu_w (Asm_raw.amomaxu_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"amoswap.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `amoswap_w (Asm_raw.amoswap_w ~rd ~rs1 ~rs2 ~aqrl)));
  QCheck.( mk_test ~name:"lr.w" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, aqrl) -> f `lr_w (Asm_raw.lr_w ~rd ~rs1 ~aqrl)));
  QCheck.( mk_test ~name:"sc.w" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 32) (int 32) (int 32) (int 4)) 
    (fun (rd, rs1, rs2, aqrl) -> f `sc_w (Asm_raw.sc_w ~rd ~rs1 ~rs2 ~aqrl)));
]

end

