module T = struct

type t = [
| `beq
| `bne
| `blt
| `bge
| `bltu
| `bgeu
| `jalr
| `jal
| `lui
| `auipc
| `addi
| `slli
| `slti
| `sltiu
| `xori
| `srli
| `srai
| `ori
| `andi
| `add
| `sub
| `sll
| `slt
| `sltu
| `xor_
| `srl
| `sra
| `or_
| `and_
| `lb
| `lh
| `lw
| `lbu
| `lhu
| `sb
| `sh
| `sw
| `fence
| `fence_i
]

let mask_match = [
  `beq     , (0x0000707fl,0x00000063l);
  `bne     , (0x0000707fl,0x00001063l);
  `blt     , (0x0000707fl,0x00004063l);
  `bge     , (0x0000707fl,0x00005063l);
  `bltu    , (0x0000707fl,0x00006063l);
  `bgeu    , (0x0000707fl,0x00007063l);
  `jalr    , (0x0000707fl,0x00000067l);
  `jal     , (0x0000007fl,0x0000006fl);
  `lui     , (0x0000007fl,0x00000037l);
  `auipc   , (0x0000007fl,0x00000017l);
  `addi    , (0x0000707fl,0x00000013l);
  `slli    , (0xfc00707fl,0x00001013l);
  `slti    , (0x0000707fl,0x00002013l);
  `sltiu   , (0x0000707fl,0x00003013l);
  `xori    , (0x0000707fl,0x00004013l);
  `srli    , (0xfc00707fl,0x00005013l);
  `srai    , (0xfc00707fl,0x40005013l);
  `ori     , (0x0000707fl,0x00006013l);
  `andi    , (0x0000707fl,0x00007013l);
  `add     , (0xfe00707fl,0x00000033l);
  `sub     , (0xfe00707fl,0x40000033l);
  `sll     , (0xfe00707fl,0x00001033l);
  `slt     , (0xfe00707fl,0x00002033l);
  `sltu    , (0xfe00707fl,0x00003033l);
  `xor_    , (0xfe00707fl,0x00004033l);
  `srl     , (0xfe00707fl,0x00005033l);
  `sra     , (0xfe00707fl,0x40005033l);
  `or_     , (0xfe00707fl,0x00006033l);
  `and_    , (0xfe00707fl,0x00007033l);
  `lb      , (0x0000707fl,0x00000003l);
  `lh      , (0x0000707fl,0x00001003l);
  `lw      , (0x0000707fl,0x00002003l);
  `lbu     , (0x0000707fl,0x00004003l);
  `lhu     , (0x0000707fl,0x00005003l);
  `sb      , (0x0000707fl,0x00000023l);
  `sh      , (0x0000707fl,0x00001023l);
  `sw      , (0x0000707fl,0x00002023l);
  `fence   , (0x0000707fl,0x0000000fl);
  `fence_i , (0x0000707fl,0x0000100fl);
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
  | `beq      ->
    ("beq" ^ " bimm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " bimm12lo=" ^ (x 11 7))
  | `bne      ->
    ("bne" ^ " bimm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " bimm12lo=" ^ (x 11 7))
  | `blt      ->
    ("blt" ^ " bimm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " bimm12lo=" ^ (x 11 7))
  | `bge      ->
    ("bge" ^ " bimm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " bimm12lo=" ^ (x 11 7))
  | `bltu     ->
    ("bltu" ^ " bimm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " bimm12lo=" ^ (x 11 7))
  | `bgeu     ->
    ("bgeu" ^ " bimm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " bimm12lo=" ^ (x 11 7))
  | `jalr     ->
    ("jalr" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `jal      ->
    ("jal" ^ " rd=" ^ (x 11 7) ^ " jimm20=" ^ (x 31 12))
  | `lui      ->
    ("lui" ^ " rd=" ^ (x 11 7) ^ " imm20=" ^ (x 31 12))
  | `auipc    ->
    ("auipc" ^ " rd=" ^ (x 11 7) ^ " imm20=" ^ (x 31 12))
  | `addi     ->
    ("addi" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `slli     ->
    ("slli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `slti     ->
    ("slti" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `sltiu    ->
    ("sltiu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `xori     ->
    ("xori" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `srli     ->
    ("srli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `srai     ->
    ("srai" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamt=" ^ (x 25 20))
  | `ori      ->
    ("ori" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `andi     ->
    ("andi" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `add      ->
    ("add" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sub      ->
    ("sub" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sll      ->
    ("sll" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `slt      ->
    ("slt" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sltu     ->
    ("sltu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `xor_     ->
    ("xor" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `srl      ->
    ("srl" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `sra      ->
    ("sra" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `or_      ->
    ("or" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `and_     ->
    ("and" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20))
  | `lb       ->
    ("lb" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `lh       ->
    ("lh" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `lw       ->
    ("lw" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `lbu      ->
    ("lbu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `lhu      ->
    ("lhu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `sb       ->
    ("sb" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `sh       ->
    ("sh" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `sw       ->
    ("sw" ^ " imm12hi=" ^ (x 31 25) ^ " rs1=" ^ (x 19 15) ^ " rs2=" ^ (x 24 20) ^ " imm12lo=" ^ (x 11 7))
  | `fence    ->
    ("fence" ^ " pred=" ^ (x 27 24) ^ " succ=" ^ (x 23 20))
  | `fence_i  ->
    ("fence.i")
end

module Asm = struct

let beq ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (((of_int bimm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int bimm12lo) &: 0x1fl) <<: 7) |:
  0x63l)

let bne ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (((of_int bimm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int bimm12lo) &: 0x1fl) <<: 7) |:
  0x1063l)

let blt ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (((of_int bimm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int bimm12lo) &: 0x1fl) <<: 7) |:
  0x4063l)

let bge ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (((of_int bimm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int bimm12lo) &: 0x1fl) <<: 7) |:
  0x5063l)

let bltu ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (((of_int bimm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int bimm12lo) &: 0x1fl) <<: 7) |:
  0x6063l)

let bgeu ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (((of_int bimm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int bimm12lo) &: 0x1fl) <<: 7) |:
  0x7063l)

let jalr ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x67l)

let jal ~rd ~jimm20 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int jimm20) &: 0xfffffl) <<: 12) |:
  0x6fl)

let lui ~rd ~imm20 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int imm20) &: 0xfffffl) <<: 12) |:
  0x37l)

let auipc ~rd ~imm20 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int imm20) &: 0xfffffl) <<: 12) |:
  0x17l)

let addi ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x13l)

let slli ~rd ~rs1 ~shamt = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamt) &: 0x3fl) <<: 20) |:
  0x1013l)

let slti ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x2013l)

let sltiu ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x3013l)

let xori ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x4013l)

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

let ori ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x6013l)

let andi ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x7013l)

let add ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x33l)

let sub ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x40000033l)

let sll ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x1033l)

let slt ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x2033l)

let sltu ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x3033l)

let xor_ ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x4033l)

let srl ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x5033l)

let sra ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x40005033l)

let or_ ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x6033l)

let and_ ~rd ~rs1 ~rs2 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  0x7033l)

let lb ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x3l)

let lh ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x1003l)

let lw ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x2003l)

let lbu ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x4003l)

let lhu ~rd ~rs1 ~imm12 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int imm12) &: 0xfffl) <<: 20) |:
  0x5003l)

let sb ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x23l)

let sh ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x1023l)

let sw ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (((of_int imm12hi) &: 0x7fl) <<: 25) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int rs2) &: 0x1fl) <<: 20) |:
  (((of_int imm12lo) &: 0x1fl) <<: 7) |:
  0x2023l)

let fence ~pred ~succ = Types.I.(
  (((of_int pred) &: 0xfl) <<: 24) |:
  (((of_int succ) &: 0xfl) <<: 20) |:
  0xfl)

let fence_i = Types.I.(
  0x100fl)

end

