module T = struct

type t = [
| `scall
| `sbreak
| `sret
| `csrrw
| `csrrs
| `csrrc
| `csrrwi
| `csrrsi
| `csrrci
]

let mask_match = [
  `scall   , (0xffffffffl,0x00000073l);
  `sbreak  , (0xffffffffl,0x00100073l);
  `sret    , (0xffffffffl,0x80000073l);
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
  | `scall    ->
    ("scall")
  | `sbreak   ->
    ("sbreak")
  | `sret     ->
    ("sret")
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
    (`scall, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(0)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`sbreak, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(1)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`sret, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(2048)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrs, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(2)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrc, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrwi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrsi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrci, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(7)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
  ]

end

module Asm = struct

let scall = Types.I.(
  0x73l)

let sbreak = Types.I.(
  0x100073l)

let sret = Types.I.(
  0x80000073l)

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

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"csrrw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrw (Asm.csrrw ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrs" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrs (Asm.csrrs ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrc" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrc (Asm.csrrc ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrwi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrwi (Asm.csrrwi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrsi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrsi (Asm.csrrsi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrci" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrci (Asm.csrrci ~rd ~rs1 ~imm12)));
]

end

