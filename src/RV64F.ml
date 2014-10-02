module T = struct

type t = [
| `fcvt_l_s
| `fcvt_lu_s
| `fcvt_s_l
| `fcvt_s_lu
]

let mask_match = [
  `fcvt_l_s, (0xfff0007fl,0xc0200053l);
  `fcvt_lu_s, (0xfff0007fl,0xc0300053l);
  `fcvt_s_l, (0xfff0007fl,0xd0200053l);
  `fcvt_s_lu, (0xfff0007fl,0xd0300053l);
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
  | `fcvt_l_s ->
    ("fcvt.l.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_lu_s ->
    ("fcvt.lu.s" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_s_l ->
    ("fcvt.s.l" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
  | `fcvt_s_lu ->
    ("fcvt.s.lu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rm=" ^ (x 14 12))
let fields =
  let open Types.Fields in
  [
    (`fcvt_l_s, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(2)); Range((31,27),Int(24)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(0)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_lu_s, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(3)); Range((31,27),Int(24)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(0)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_s_l, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(2)); Range((31,27),Int(26)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(0)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
    (`fcvt_s_lu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((24,20),Int(3)); Range((31,27),Int(26)); Field((`rm,"rm",(14,12)), Nothing); Range((26,25),Int(0)); Range((6,2),Int(20)); Range((1,0),Int(3)); ]);
  ]

end

module Asm = struct

let fcvt_l_s ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc0200053l)

let fcvt_lu_s ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc0300053l)

let fcvt_s_l ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd0200053l)

let fcvt_s_lu ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd0300053l)

end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"fcvt.l.s" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_l_s (Asm.fcvt_l_s ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.lu.s" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_lu_s (Asm.fcvt_lu_s ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.s.l" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_s_l (Asm.fcvt_s_l ~rd ~rs1 ~rm)));
  QCheck.( mk_test ~name:"fcvt.s.lu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 8)) 
    (fun (rd, rs1, rm) -> f `fcvt_s_lu (Asm.fcvt_s_lu ~rd ~rs1 ~rm)));
]

end

