module T = struct

type t = [
| `lwu
| `ld
| `sd
| `slli
| `srli
| `srai
| `addiw
| `slliw
| `srliw
| `sraiw
| `addw
| `subw
| `sllw
| `srlw
| `sraw
]

let mask_match = [
  `lwu     , (0x0000707fl,0x00006003l);
  `ld      , (0x0000707fl,0x00003003l);
  `sd      , (0x0000707fl,0x00003023l);
  `slli    , (0xfc00707fl,0x00001013l);
  `srli    , (0xfc00707fl,0x00005013l);
  `srai    , (0xfc00707fl,0x40005013l);
  `addiw   , (0x0000707fl,0x0000001bl);
  `slliw   , (0xfe00707fl,0x0000101bl);
  `srliw   , (0xfe00707fl,0x0000501bl);
  `sraiw   , (0xfe00707fl,0x4000501bl);
  `addw    , (0xfe00707fl,0x0000003bl);
  `subw    , (0xfe00707fl,0x4000003bl);
  `sllw    , (0xfe00707fl,0x0000103bl);
  `srlw    , (0xfe00707fl,0x0000503bl);
  `sraw    , (0xfe00707fl,0x4000503bl);
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
  | `lwu      ->
    ("lwu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `ld       ->
    ("ld" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `sd       ->
    ("sd" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `slli     ->
    ("slli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `srli     ->
    ("srli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `srai     ->
    ("srai" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `addiw    ->
    ("addiw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `slliw    ->
    ("slliw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `srliw    ->
    ("srliw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `sraiw    ->
    ("sraiw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `addw     ->
    ("addw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `subw     ->
    ("subw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sllw     ->
    ("sllw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `srlw     ->
    ("srlw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sraw     ->
    ("sraw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
end

module Asm = struct

let lwu ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x6003l)

let ld ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x3003l)

let sd ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x3023l)

let slli ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x1013l)

let srli ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x5013l)

let srai ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x40005013l)

let addiw ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x1bl)

let slliw ~rd ~rs1 ~shamtw = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamtw) &: 0x1fl) <<: 20) |:
  0x101bl)

let srliw ~rd ~rs1 ~shamtw = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamtw) &: 0x1fl) <<: 20) |:
  0x501bl)

let sraiw ~rd ~rs1 ~shamtw = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamtw) &: 0x1fl) <<: 20) |:
  0x4000501bl)

let addw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x3bl)

let subw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x4000003bl)

let sllw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x103bl)

let srlw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x503bl)

let sraw ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x4000503bl)

end

