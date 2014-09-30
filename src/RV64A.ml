module T = struct

type t = [
| `amoadd_d
| `amoxor_d
| `amoor_d
| `amoand_d
| `amomin_d
| `amomax_d
| `amominu_d
| `amomaxu_d
| `amoswap_d
| `lr_d
| `sc_d
]

let mask_match = [
  `amoadd_d, (0xf800707fl,0x0000302fl);
  `amoxor_d, (0xf800707fl,0x2000302fl);
  `amoor_d , (0xf800707fl,0x4000302fl);
  `amoand_d, (0xf800707fl,0x6000302fl);
  `amomin_d, (0xf800707fl,0x8000302fl);
  `amomax_d, (0xf800707fl,0xa000302fl);
  `amominu_d, (0xf800707fl,0xc000302fl);
  `amomaxu_d, (0xf800707fl,0xe000302fl);
  `amoswap_d, (0xf800707fl,0x0800302fl);
  `lr_d    , (0xf9f0707fl,0x1000302fl);
  `sc_d    , (0xf800707fl,0x1800302fl);
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
  | `amoadd_d ->
    ("amoadd.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoxor_d ->
    ("amoxor.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoor_d  ->
    ("amoor.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoand_d ->
    ("amoand.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amomin_d ->
    ("amomin.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amomax_d ->
    ("amomax.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amominu_d ->
    ("amominu.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amomaxu_d ->
    ("amomaxu.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `amoswap_d ->
    ("amoswap.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
  | `lr_d     ->
    ("lr.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " aqrl=" ^ (x 26 25))
  | `sc_d     ->
    ("sc.d" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " aqrl=" ^ (x 26 25))
end

module Asm = struct

let amoadd_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x302fl)

let amoxor_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x2000302fl)

let amoor_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x4000302fl)

let amoand_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x6000302fl)

let amomin_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x8000302fl)

let amomax_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0xa000302fl)

let amominu_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0xc000302fl)

let amomaxu_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0xe000302fl)

let amoswap_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x800302fl)

let lr_d ~rd ~rs1 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x1000302fl)

let sc_d ~rd ~rs1 ~rs2 ~aqrl = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int aqrl) &: 0x3l) <<: 25) |:
  0x1800302fl)

end

