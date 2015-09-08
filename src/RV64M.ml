module T = struct

type t = [
| `mulw
| `divw
| `divuw
| `remw
| `remuw
]

let mask_match = [
  `mulw    , (0xfe00707fl,0x0200003bl);
  `divw    , (0xfe00707fl,0x0200403bl);
  `divuw   , (0xfe00707fl,0x0200503bl);
  `remw    , (0xfe00707fl,0x0200603bl);
  `remuw   , (0xfe00707fl,0x0200703bl);
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
  | `mulw     ->
    ("mulw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `divw     ->
    ("divw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `divuw    ->
    ("divuw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `remw     ->
    ("remw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `remuw    ->
    ("remuw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
let fields =
  let open Types.Fields in
  [
    (`mulw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(0)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`divw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(4)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`divuw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(5)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`remw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(6)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
    (`remuw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(7)); Range((6,2),Int(14)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let mulw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x200003bl)

let divw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x200403bl)

let divuw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x200503bl)

let remw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x200603bl)

let remuw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x200703bl)

end

module Asm = struct

let mulw = Asm_raw.mulw
let divw = Asm_raw.divw
let divuw = Asm_raw.divuw
let remw = Asm_raw.remw
let remuw = Asm_raw.remuw
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"mulw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `mulw (Asm_raw.mulw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"divw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `divw (Asm_raw.divw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"divuw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `divuw (Asm_raw.divuw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"remw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `remw (Asm_raw.remw ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"remuw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `remuw (Asm_raw.remuw ~rd ~rs1 ~rs2)));
]

end

