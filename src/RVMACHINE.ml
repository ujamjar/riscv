module T = struct

type t = [
| `csrrw
| `csrrc
| `csrrwi
| `csrrsi
| `csrrci
| `eret
| `wfi
] deriving(Enum,Bounded,Show)

let name = "rvmachine"

let mask_match = [
  `csrrw   , (0x0000707fl,0x00001073l);
  `csrrc   , (0x0000707fl,0x00003073l);
  `csrrwi  , (0x0000707fl,0x00005073l);
  `csrrsi  , (0x0000707fl,0x00006073l);
  `csrrci  , (0x0000707fl,0x00007073l);
  `eret    , (0xffffffffl,0x10000073l);
  `wfi     , (0xffffffffl,0x10200073l);
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
  | `csrrw    ->
    ("csrrw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrc    ->
    ("csrrc" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrwi   ->
    ("csrrwi" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrsi   ->
    ("csrrsi" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `csrrci   ->
    ("csrrci" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `eret     ->
    ("eret")
  | `wfi      ->
    ("wfi")
let fields =
  let open Types.Fields in
  [
    (`csrrw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrc, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrwi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrsi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrci, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(7)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`eret, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(256)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`wfi, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(258)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let csrrw ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x1073l)

let csrrc ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x3073l)

let csrrwi ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x5073l)

let csrrsi ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x6073l)

let csrrci ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x7073l)

let eret = Types.I.(
  0x10000073l)

let wfi = Types.I.(
  0x10200073l)

end

module Asm = struct

let csrrw ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrw ~rd ~rs1 ) ~imm
let csrrc ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrc ~rd ~rs1 ) ~imm
let csrrwi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrwi ~rd ~rs1 ) ~imm
let csrrsi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrsi ~rd ~rs1 ) ~imm
let csrrci ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrci ~rd ~rs1 ) ~imm
let eret = Asm_raw.eret
let wfi = Asm_raw.wfi
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"csrrw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrw (Asm_raw.csrrw ~rd ~rs1 ~imm12)));
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

