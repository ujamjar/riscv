module T = struct

type t = [
| `_custom0
| `_custom0_rs1
| `_custom0_rs1_rs2
| `_custom0_rd
| `_custom0_rd_rs1
| `_custom0_rd_rs1_rs2
| `_custom1
| `_custom1_rs1
| `_custom1_rs1_rs2
| `_custom1_rd
| `_custom1_rd_rs1
| `_custom1_rd_rs1_rs2
| `_custom2
| `_custom2_rs1
| `_custom2_rs1_rs2
| `_custom2_rd
| `_custom2_rd_rs1
| `_custom2_rd_rs1_rs2
| `_custom3
| `_custom3_rs1
| `_custom3_rs1_rs2
| `_custom3_rd
| `_custom3_rd_rs1
| `_custom3_rd_rs1_rs2
]

let mask_match = [
  `_custom0, (0x0000707fl,0x0000000bl);
  `_custom0_rs1, (0x0000707fl,0x0000200bl);
  `_custom0_rs1_rs2, (0x0000707fl,0x0000300bl);
  `_custom0_rd, (0x0000707fl,0x0000400bl);
  `_custom0_rd_rs1, (0x0000707fl,0x0000600bl);
  `_custom0_rd_rs1_rs2, (0x0000707fl,0x0000700bl);
  `_custom1, (0x0000707fl,0x0000002bl);
  `_custom1_rs1, (0x0000707fl,0x0000202bl);
  `_custom1_rs1_rs2, (0x0000707fl,0x0000302bl);
  `_custom1_rd, (0x0000707fl,0x0000402bl);
  `_custom1_rd_rs1, (0x0000707fl,0x0000602bl);
  `_custom1_rd_rs1_rs2, (0x0000707fl,0x0000702bl);
  `_custom2, (0x0000707fl,0x0000005bl);
  `_custom2_rs1, (0x0000707fl,0x0000205bl);
  `_custom2_rs1_rs2, (0x0000707fl,0x0000305bl);
  `_custom2_rd, (0x0000707fl,0x0000405bl);
  `_custom2_rd_rs1, (0x0000707fl,0x0000605bl);
  `_custom2_rd_rs1_rs2, (0x0000707fl,0x0000705bl);
  `_custom3, (0x0000707fl,0x0000007bl);
  `_custom3_rs1, (0x0000707fl,0x0000207bl);
  `_custom3_rs1_rs2, (0x0000707fl,0x0000307bl);
  `_custom3_rd, (0x0000707fl,0x0000407bl);
  `_custom3_rd_rs1, (0x0000707fl,0x0000607bl);
  `_custom3_rd_rs1_rs2, (0x0000707fl,0x0000707bl);
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
  | `_custom0 ->
    ("@custom0" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom0_rs1 ->
    ("@custom0.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom0_rs1_rs2 ->
    ("@custom0.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom0_rd ->
    ("@custom0.rd" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom0_rd_rs1 ->
    ("@custom0.rd.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom0_rd_rs1_rs2 ->
    ("@custom0.rd.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom1 ->
    ("@custom1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom1_rs1 ->
    ("@custom1.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom1_rs1_rs2 ->
    ("@custom1.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom1_rd ->
    ("@custom1.rd" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom1_rd_rs1 ->
    ("@custom1.rd.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom1_rd_rs1_rs2 ->
    ("@custom1.rd.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom2 ->
    ("@custom2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom2_rs1 ->
    ("@custom2.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom2_rs1_rs2 ->
    ("@custom2.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom2_rd ->
    ("@custom2.rd" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom2_rd_rs1 ->
    ("@custom2.rd.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom2_rd_rs1_rs2 ->
    ("@custom2.rd.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom3 ->
    ("@custom3" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom3_rs1 ->
    ("@custom3.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom3_rs1_rs2 ->
    ("@custom3.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom3_rd ->
    ("@custom3.rd" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom3_rd_rs1 ->
    ("@custom3.rd.rs1" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `_custom3_rd_rs1_rs2 ->
    ("@custom3.rd.rs1.rs2" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
end

module Asm = struct

let _custom0 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0xbl)

let _custom0_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x200bl)

let _custom0_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x300bl)

let _custom0_rd ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x400bl)

let _custom0_rd_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x600bl)

let _custom0_rd_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x700bl)

let _custom1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x2bl)

let _custom1_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x202bl)

let _custom1_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x302bl)

let _custom1_rd ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x402bl)

let _custom1_rd_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x602bl)

let _custom1_rd_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x702bl)

let _custom2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x5bl)

let _custom2_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x205bl)

let _custom2_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x305bl)

let _custom2_rd ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x405bl)

let _custom2_rd_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x605bl)

let _custom2_rd_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x705bl)

let _custom3 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x7bl)

let _custom3_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x207bl)

let _custom3_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x307bl)

let _custom3_rd ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x407bl)

let _custom3_rd_rs1 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x607bl)

let _custom3_rd_rs1_rs2 ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x707bl)

end

