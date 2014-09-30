module T = struct

type t = [
| `fcvt_l_d
| `fcvt_lu_d
| `fmv_x_d
| `fcvt_d_l
| `fcvt_d_lu
| `fmv_d_x
]

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
end

module Asm = struct

let fcvt_l_d ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc2200053l)

let fcvt_lu_d ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xc2300053l)

let fmv_x_d ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0xe2000053l)

let fcvt_d_l ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd2200053l)

let fcvt_d_lu ~rd ~rs1 ~rm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rm) &: 0x7l) <<: 12) |:
  0xd2300053l)

let fmv_d_x ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0xf2000053l)

end

