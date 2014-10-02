module T = struct

type t = [
| `mul
| `mulh
| `mulhsu
| `mulhu
| `div
| `divu
| `rem
| `remu
]

let mask_match = [
  `mul     , (0xfe00707fl,0x02000033l);
  `mulh    , (0xfe00707fl,0x02001033l);
  `mulhsu  , (0xfe00707fl,0x02002033l);
  `mulhu   , (0xfe00707fl,0x02003033l);
  `div     , (0xfe00707fl,0x02004033l);
  `divu    , (0xfe00707fl,0x02005033l);
  `rem     , (0xfe00707fl,0x02006033l);
  `remu    , (0xfe00707fl,0x02007033l);
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
  | `mul      ->
    ("mul" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `mulh     ->
    ("mulh" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `mulhsu   ->
    ("mulhsu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `mulhu    ->
    ("mulhu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `div      ->
    ("div" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `divu     ->
    ("divu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `rem      ->
    ("rem" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `remu     ->
    ("remu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
let fields =
  let open Types.Fields in
  [
    (`mul, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(0)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`mulh, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(1)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`mulhsu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(2)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`mulhu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(3)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`div, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(4)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`divu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(5)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`rem, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(6)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`remu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(1)); Range((14,12),Int(7)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
  ]

end

module Asm = struct

let mul ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2000033l)

let mulh ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2001033l)

let mulhsu ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2002033l)

let mulhu ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2003033l)

let div ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2004033l)

let divu ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2005033l)

let rem ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2006033l)

let remu ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2007033l)

end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"mul" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `mul (Asm.mul ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"mulh" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `mulh (Asm.mulh ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"mulhsu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `mulhsu (Asm.mulhsu ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"mulhu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `mulhu (Asm.mulhu ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"div" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `div (Asm.div ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"divu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `divu (Asm.divu ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"rem" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `rem (Asm.rem ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"remu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `remu (Asm.remu ~rd ~rs1 ~rs2)));
]

end

