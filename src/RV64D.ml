module T = struct

type t = [
| `fcvt_l_d
| `fcvt_lu_d
| `fmv_x_d
| `fcvt_d_l
| `fcvt_d_lu
| `fmv_d_x
] deriving(Enum,Bounded,Show)

let name = "rv64d"

let mask_match = [
  `fcvt_l_d, (0xfff0007fl,0xc2200053l);
  `fcvt_lu_d, (0xfff0007fl,0xc2300053l);
  `fmv_x_d , (0xfff0707fl,0xe2000053l);
  `fcvt_d_l, (0xfff0007fl,0xd2200053l);
  `fcvt_d_lu, (0xfff0007fl,0xd2300053l);
  `fmv_d_x , (0xfff0707fl,0xf2000053l);
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
  | `fcvt_l_d ->
    ("fcvt.l.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_lu_d ->
    ("fcvt.lu.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fmv_x_d  ->
    ("fmv.x.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `fcvt_d_l ->
    ("fcvt.d.l" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_d_lu ->
    ("fcvt.d.lu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fmv_d_x  ->
    ("fmv.d.x" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
let fields =
  let open Types.Fields in
  [
    (`fcvt_l_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(2)); Range((31,27),Int(24)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_lu_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(3)); Range((31,27),Int(24)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fmv_x_d, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(28)); Range((14,12),Int(0)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_d_l, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(2)); Range((31,27),Int(26)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_d_lu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(3)); Range((31,27),Int(26)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fmv_d_x, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(0)); Range((31,27),Int(30)); Range((14,12),Int(0)); Range((26,25),Int(1)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let fcvt_l_d ~rd ~rs1 ~rm = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rm) &: 0x7l) 12) |:
  0xc2200053l)

let fcvt_lu_d ~rd ~rs1 ~rm = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rm) &: 0x7l) 12) |:
  0xc2300053l)

let fmv_x_d ~rd ~rs1 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  0xe2000053l)

let fcvt_d_l ~rd ~rs1 ~rm = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rm) &: 0x7l) 12) |:
  0xd2200053l)

let fcvt_d_lu ~rd ~rs1 ~rm = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rm) &: 0x7l) 12) |:
  0xd2300053l)

let fmv_d_x ~rd ~rs1 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  0xf2000053l)

end

module Asm = struct

let fcvt_l_d = Asm_raw.fcvt_l_d
let fcvt_lu_d = Asm_raw.fcvt_lu_d
let fmv_x_d = Asm_raw.fmv_x_d
let fcvt_d_l = Asm_raw.fcvt_d_l
let fcvt_d_lu = Asm_raw.fcvt_d_lu
let fmv_d_x = Asm_raw.fmv_d_x
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"fcvt.l.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_l_d (Asm_raw.fcvt_l_d ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.lu.d" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_lu_d (Asm_raw.fcvt_lu_d ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fmv.x.d" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (rd, rs1) -> f `fmv_x_d (Asm_raw.fmv_x_d ~rd ~rs1)));
  QCheck.( mk_test ~name:"fcvt.d.l" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_d_l (Asm_raw.fcvt_d_l ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.d.lu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_d_lu (Asm_raw.fcvt_d_lu ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fmv.d.x" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (rd, rs1) -> f `fmv_d_x (Asm_raw.fmv_d_x ~rd ~rs1)));
]

end

