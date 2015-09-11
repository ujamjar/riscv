module T = struct

type t = [
| `sret
| `sfence_vm
| `wfi
| `mrth
| `mrts
| `hrts
| `csrrw
| `csrrs
| `csrrc
| `csrrwi
| `csrrsi
| `csrrci
]

let mask_match = [
  `sret    , (0xffffffffl,0x10000073l);
  `sfence_vm, (0xfff07fffl,0x10100073l);
  `wfi     , (0xffffffffl,0x10200073l);
  `mrth    , (0xffffffffl,0x30600073l);
  `mrts    , (0xffffffffl,0x30500073l);
  `hrts    , (0xffffffffl,0x20500073l);
  `csrrw   , (0x0000707fl,0x00001073l);
  `csrrs   , (0x0000707fl,0x00002073l);
  `csrrc   , (0x0000707fl,0x00003073l);
  `csrrwi  , (0x0000707fl,0x00005073l);
  `csrrsi  , (0x0000707fl,0x00006073l);
  `csrrci  , (0x0000707fl,0x00007073l);
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
  | `sret     ->
    ("sret")
  | `sfence_vm ->
    ("sfence.vm" ^ " rs1=" ^ (x 19 15))
  | `wfi      ->
    ("wfi")
  | `mrth     ->
    ("mrth")
  | `mrts     ->
    ("mrts")
  | `hrts     ->
    ("hrts")
  | `csrrw    ->
    ("csrrw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrs    ->
    ("csrrs" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrc    ->
    ("csrrc" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrwi   ->
    ("csrrwi" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrsi   ->
    ("csrrsi" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrci   ->
    ("csrrci" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
let fields =
  let open Types.Fields in
  [
    (`sret, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(256)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`sfence_vm, [ Range((11,7),Int(0)); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,20),Int(257)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`wfi, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(258)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`mrth, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(774)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`mrts, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(773)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`hrts, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(517)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrs, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(2)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrc, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrwi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrsi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrci, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(7)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let sret = Types.I.(
  0x10000073l)

let sfence_vm ~rs1 = Types.I.(
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0x10100073l)

let wfi = Types.I.(
  0x10200073l)

let mrth = Types.I.(
  0x30600073l)

let mrts = Types.I.(
  0x30500073l)

let hrts = Types.I.(
  0x20500073l)

let csrrw ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x1073l)

let csrrs ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x2073l)

let csrrc ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x3073l)

let csrrwi ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x5073l)

let csrrsi ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x6073l)

let csrrci ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x7073l)

end

module Asm = struct

let sret = Asm_raw.sret
let sfence_vm = Asm_raw.sfence_vm
let wfi = Asm_raw.wfi
let mrth = Asm_raw.mrth
let mrts = Asm_raw.mrts
let hrts = Asm_raw.hrts
let csrrw ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrw ~rd ~rs1 ) ~imm
let csrrs ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrs ~rd ~rs1 ) ~imm
let csrrc ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrc ~rd ~rs1 ) ~imm
let csrrwi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrwi ~rd ~rs1 ) ~imm
let csrrsi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrsi ~rd ~rs1 ) ~imm
let csrrci ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrci ~rd ~rs1 ) ~imm
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"sfence.vm" ~n 
    ~pp:PP.(QCRV.PP.tuple1 int) ~limit:2
    Arbitrary.(QCRV.tuple1 (int 32)) 
    (fun (rs1) -> f `sfence_vm (Asm_raw.sfence_vm ~rs1)));
  QCheck.( mk_test ~name:"csrrw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrw (Asm_raw.csrrw ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrs" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrs (Asm_raw.csrrs ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrc" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrc (Asm_raw.csrrc ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrwi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrwi (Asm_raw.csrrwi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrsi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrsi (Asm_raw.csrrsi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrci" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrci (Asm_raw.csrrci ~rd ~rs1 ~imm12)));
]

end

