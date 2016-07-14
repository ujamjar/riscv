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
| `slti
| `sltiu
| `xori
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
| `slli
| `srli
| `srai
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
| `ecall
| `ebreak
| `eret
| `wfi
| `csrrw
| `csrrs
| `csrrc
| `csrrwi
| `csrrsi
| `csrrci
] deriving(Enum,Bounded,Show)

let name = "rv32i_machine"

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
  `slti    , (0x0000707fl,0x00002013l);
  `sltiu   , (0x0000707fl,0x00003013l);
  `xori    , (0x0000707fl,0x00004013l);
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
  `slli    , (0xfe00707fl,0x00001013l);
  `srli    , (0xfe00707fl,0x00005013l);
  `srai    , (0xfe00707fl,0x40005013l);
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
  `ecall   , (0xffffffffl,0x00000073l);
  `ebreak  , (0xffffffffl,0x00100073l);
  `eret    , (0xffffffffl,0x10000073l);
  `wfi     , (0xffffffffl,0x10200073l);
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
  | `slti     ->
    ("slti" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `sltiu    ->
    ("sltiu" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
  | `xori     ->
    ("xori" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " imm12=" ^ (x 31 20))
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
  | `slli     ->
    ("slli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `srli     ->
    ("srli" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `srai     ->
    ("srai" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
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
  | `ecall    ->
    ("ecall")
  | `ebreak   ->
    ("ebreak")
  | `eret     ->
    ("eret")
  | `wfi      ->
    ("wfi")
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
    (`beq, [ Field((`bimm12hi,"bimm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`bimm12lo,"bimm12lo",(11,7)), Nothing); Range((14,12),Int(0)); Range((6,2),Int(24)); Range((1,0),Int(3)); ]);
    (`bne, [ Field((`bimm12hi,"bimm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`bimm12lo,"bimm12lo",(11,7)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(24)); Range((1,0),Int(3)); ]);
    (`blt, [ Field((`bimm12hi,"bimm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`bimm12lo,"bimm12lo",(11,7)), Nothing); Range((14,12),Int(4)); Range((6,2),Int(24)); Range((1,0),Int(3)); ]);
    (`bge, [ Field((`bimm12hi,"bimm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`bimm12lo,"bimm12lo",(11,7)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(24)); Range((1,0),Int(3)); ]);
    (`bltu, [ Field((`bimm12hi,"bimm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`bimm12lo,"bimm12lo",(11,7)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(24)); Range((1,0),Int(3)); ]);
    (`bgeu, [ Field((`bimm12hi,"bimm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`bimm12lo,"bimm12lo",(11,7)), Nothing); Range((14,12),Int(7)); Range((6,2),Int(24)); Range((1,0),Int(3)); ]);
    (`jalr, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(0)); Range((6,2),Int(25)); Range((1,0),Int(3)); ]);
    (`jal, [ Field((`rd,"rd",(11,7)), Nothing); Field((`jimm20,"jimm20",(31,12)), Nothing); Range((6,2),Int(27)); Range((1,0),Int(3)); ]);
    (`lui, [ Field((`rd,"rd",(11,7)), Nothing); Field((`imm20,"imm20",(31,12)), Nothing); Range((6,2),Int(13)); Range((1,0),Int(3)); ]);
    (`auipc, [ Field((`rd,"rd",(11,7)), Nothing); Field((`imm20,"imm20",(31,12)), Nothing); Range((6,2),Int(5)); Range((1,0),Int(3)); ]);
    (`addi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(0)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`slti, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(2)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`sltiu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`xori, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(4)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`ori, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`andi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(7)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`add, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(0)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`sub, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(32)); Range((14,12),Int(0)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`sll, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(1)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`slt, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(2)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`sltu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(3)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`xor_, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(4)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`srl, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(5)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`sra, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(32)); Range((14,12),Int(5)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`or_, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(6)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`and_, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Range((31,25),Int(0)); Range((14,12),Int(7)); Range((6,2),Int(12)); Range((1,0),Int(3)); ]);
    (`slli, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,25),Int(0)); Field((`shamtw,"shamtw",(24,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`srli, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,25),Int(0)); Field((`shamtw,"shamtw",(24,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`srai, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,25),Int(32)); Field((`shamtw,"shamtw",(24,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(4)); Range((1,0),Int(3)); ]);
    (`lb, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(0)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`lh, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`lw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(2)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`lbu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(4)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`lhu, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(0)); Range((1,0),Int(3)); ]);
    (`sb, [ Field((`imm12hi,"imm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`imm12lo,"imm12lo",(11,7)), Nothing); Range((14,12),Int(0)); Range((6,2),Int(8)); Range((1,0),Int(3)); ]);
    (`sh, [ Field((`imm12hi,"imm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`imm12lo,"imm12lo",(11,7)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(8)); Range((1,0),Int(3)); ]);
    (`sw, [ Field((`imm12hi,"imm12hi",(31,25)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`rs2,"rs2",(24,20)), Nothing); Field((`imm12lo,"imm12lo",(11,7)), Nothing); Range((14,12),Int(2)); Range((6,2),Int(8)); Range((1,0),Int(3)); ]);
    (`fence, [ Range((31,28),Ignore); Field((`pred,"pred",(27,24)), Nothing); Field((`succ,"succ",(23,20)), Nothing); Range((19,15),Ignore); Range((14,12),Int(0)); Range((11,7),Ignore); Range((6,2),Int(3)); Range((1,0),Int(3)); ]);
    (`fence_i, [ Range((31,28),Ignore); Range((27,20),Ignore); Range((19,15),Ignore); Range((14,12),Int(1)); Range((11,7),Ignore); Range((6,2),Int(3)); Range((1,0),Int(3)); ]);
    (`ecall, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(0)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`ebreak, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(1)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`eret, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(256)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`wfi, [ Range((11,7),Int(0)); Range((19,15),Int(0)); Range((31,20),Int(258)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrw, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(1)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrs, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(2)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrc, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(3)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrwi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(5)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrsi, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(6)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
    (`csrrci, [ Field((`rd,"rd",(11,7)), Nothing); Field((`rs1,"rs1",(19,15)), Nothing); Field((`imm12,"imm12",(31,20)), Nothing); Range((14,12),Int(7)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let beq ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (sll ((of_int bimm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int bimm12lo) &: 0x1fl) 7) |:
  0x63l)

let bne ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (sll ((of_int bimm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int bimm12lo) &: 0x1fl) 7) |:
  0x1063l)

let blt ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (sll ((of_int bimm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int bimm12lo) &: 0x1fl) 7) |:
  0x4063l)

let bge ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (sll ((of_int bimm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int bimm12lo) &: 0x1fl) 7) |:
  0x5063l)

let bltu ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (sll ((of_int bimm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int bimm12lo) &: 0x1fl) 7) |:
  0x6063l)

let bgeu ~bimm12hi ~rs1 ~rs2 ~bimm12lo = Types.I.(
  (sll ((of_int bimm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int bimm12lo) &: 0x1fl) 7) |:
  0x7063l)

let jalr ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x67l)

let jal ~rd ~jimm20 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int jimm20) &: 0xfffffl) 12) |:
  0x6fl)

let lui ~rd ~imm20 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int imm20) &: 0xfffffl) 12) |:
  0x37l)

let auipc ~rd ~imm20 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int imm20) &: 0xfffffl) 12) |:
  0x17l)

let addi ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x13l)

let slti ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x2013l)

let sltiu ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x3013l)

let xori ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x4013l)

let ori ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x6013l)

let andi ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x7013l)

let add ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x33l)

let sub ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x40000033l)

let sll ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x1033l)

let slt ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x2033l)

let sltu ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x3033l)

let xor_ ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x4033l)

let srl ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x5033l)

let sra ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x40005033l)

let or_ ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x6033l)

let and_ ~rd ~rs1 ~rs2 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  0x7033l)

let slli ~rd ~rs1 ~shamtw = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int shamtw) &: 0x1fl) 20) |:
  0x1013l)

let srli ~rd ~rs1 ~shamtw = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int shamtw) &: 0x1fl) 20) |:
  0x5013l)

let srai ~rd ~rs1 ~shamtw = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int shamtw) &: 0x1fl) 20) |:
  0x40005013l)

let lb ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x3l)

let lh ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x1003l)

let lw ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x2003l)

let lbu ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x4003l)

let lhu ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x5003l)

let sb ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (sll ((of_int imm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int imm12lo) &: 0x1fl) 7) |:
  0x23l)

let sh ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (sll ((of_int imm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int imm12lo) &: 0x1fl) 7) |:
  0x1023l)

let sw ~imm12hi ~rs1 ~rs2 ~imm12lo = Types.I.(
  (sll ((of_int imm12hi) &: 0x7fl) 25) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int rs2) &: 0x1fl) 20) |:
  (sll ((of_int imm12lo) &: 0x1fl) 7) |:
  0x2023l)

let fence ~pred ~succ = Types.I.(
  (sll ((of_int pred) &: 0xfl) 24) |:
  (sll ((of_int succ) &: 0xfl) 20) |:
  0xfl)

let fence_i = Types.I.(
  0x100fl)

let ecall = Types.I.(
  0x73l)

let ebreak = Types.I.(
  0x100073l)

let eret = Types.I.(
  0x10000073l)

let wfi = Types.I.(
  0x10200073l)

let csrrw ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x1073l)

let csrrs ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x2073l)

let csrrc ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x3073l)

let csrrwi ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x5073l)

let csrrsi ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x6073l)

let csrrci ~rd ~rs1 ~imm12 = Types.I.(
  (sll ((of_int rd) &: 0x1fl) 7) |:
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  (sll ((of_int imm12) &: 0xfffl) 20) |:
  0x7073l)

end

module Asm = struct

let beq ~rs1 ~rs2 ~imm = Imm.b_imm (Asm_raw.beq ~rs1 ~rs2 ) ~imm
let bne ~rs1 ~rs2 ~imm = Imm.b_imm (Asm_raw.bne ~rs1 ~rs2 ) ~imm
let blt ~rs1 ~rs2 ~imm = Imm.b_imm (Asm_raw.blt ~rs1 ~rs2 ) ~imm
let bge ~rs1 ~rs2 ~imm = Imm.b_imm (Asm_raw.bge ~rs1 ~rs2 ) ~imm
let bltu ~rs1 ~rs2 ~imm = Imm.b_imm (Asm_raw.bltu ~rs1 ~rs2 ) ~imm
let bgeu ~rs1 ~rs2 ~imm = Imm.b_imm (Asm_raw.bgeu ~rs1 ~rs2 ) ~imm
let jalr ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.jalr ~rd ~rs1 ) ~imm
let jal ~rd ~imm = Imm.j_imm (Asm_raw.jal ~rd ) ~imm
let lui ~rd ~imm = Imm.u_imm (Asm_raw.lui ~rd ) ~imm
let auipc ~rd ~imm = Imm.u_imm (Asm_raw.auipc ~rd ) ~imm
let addi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.addi ~rd ~rs1 ) ~imm
let slti ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.slti ~rd ~rs1 ) ~imm
let sltiu ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.sltiu ~rd ~rs1 ) ~imm
let xori ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.xori ~rd ~rs1 ) ~imm
let ori ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.ori ~rd ~rs1 ) ~imm
let andi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.andi ~rd ~rs1 ) ~imm
let add = Asm_raw.add
let sub = Asm_raw.sub
let sll = Asm_raw.sll
let slt = Asm_raw.slt
let sltu = Asm_raw.sltu
let xor_ = Asm_raw.xor_
let srl = Asm_raw.srl
let sra = Asm_raw.sra
let or_ = Asm_raw.or_
let and_ = Asm_raw.and_
let slli ~rd ~rs1 ~imm = Imm.shw_imm (Asm_raw.slli ~rd ~rs1 ) ~imm
let srli ~rd ~rs1 ~imm = Imm.shw_imm (Asm_raw.srli ~rd ~rs1 ) ~imm
let srai ~rd ~rs1 ~imm = Imm.shw_imm (Asm_raw.srai ~rd ~rs1 ) ~imm
let lb ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.lb ~rd ~rs1 ) ~imm
let lh ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.lh ~rd ~rs1 ) ~imm
let lw ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.lw ~rd ~rs1 ) ~imm
let lbu ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.lbu ~rd ~rs1 ) ~imm
let lhu ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.lhu ~rd ~rs1 ) ~imm
let sb ~rs1 ~rs2 ~imm = Imm.s_imm (Asm_raw.sb ~rs1 ~rs2 ) ~imm
let sh ~rs1 ~rs2 ~imm = Imm.s_imm (Asm_raw.sh ~rs1 ~rs2 ) ~imm
let sw ~rs1 ~rs2 ~imm = Imm.s_imm (Asm_raw.sw ~rs1 ~rs2 ) ~imm
let fence = Asm_raw.fence
let fence_i = Asm_raw.fence_i
let ecall = Asm_raw.ecall
let ebreak = Asm_raw.ebreak
let eret = Asm_raw.eret
let wfi = Asm_raw.wfi
let csrrw ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrw ~rd ~rs1 ) ~imm
let csrrs ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrs ~rd ~rs1 ) ~imm
let csrrc ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrc ~rd ~rs1 ) ~imm
let csrrwi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrwi ~rd ~rs1 ) ~imm
let csrrsi ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrsi ~rd ~rs1 ) ~imm
let csrrci ~rd ~rs1 ~imm = Imm.i_imm (Asm_raw.csrrci ~rd ~rs1 ) ~imm
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"beq" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (bimm12hi, rs1, rs2, bimm12lo) -> f `beq (Asm_raw.beq ~bimm12hi ~rs1 ~rs2 ~bimm12lo)));
  QCheck.( mk_test ~name:"bne" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (bimm12hi, rs1, rs2, bimm12lo) -> f `bne (Asm_raw.bne ~bimm12hi ~rs1 ~rs2 ~bimm12lo)));
  QCheck.( mk_test ~name:"blt" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (bimm12hi, rs1, rs2, bimm12lo) -> f `blt (Asm_raw.blt ~bimm12hi ~rs1 ~rs2 ~bimm12lo)));
  QCheck.( mk_test ~name:"bge" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (bimm12hi, rs1, rs2, bimm12lo) -> f `bge (Asm_raw.bge ~bimm12hi ~rs1 ~rs2 ~bimm12lo)));
  QCheck.( mk_test ~name:"bltu" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (bimm12hi, rs1, rs2, bimm12lo) -> f `bltu (Asm_raw.bltu ~bimm12hi ~rs1 ~rs2 ~bimm12lo)));
  QCheck.( mk_test ~name:"bgeu" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (bimm12hi, rs1, rs2, bimm12lo) -> f `bgeu (Asm_raw.bgeu ~bimm12hi ~rs1 ~rs2 ~bimm12lo)));
  QCheck.( mk_test ~name:"jalr" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `jalr (Asm_raw.jalr ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"jal" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 1048576)) 
    (fun (rd, jimm20) -> f `jal (Asm_raw.jal ~rd ~jimm20)));
  QCheck.( mk_test ~name:"lui" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 1048576)) 
    (fun (rd, imm20) -> f `lui (Asm_raw.lui ~rd ~imm20)));
  QCheck.( mk_test ~name:"auipc" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 1048576)) 
    (fun (rd, imm20) -> f `auipc (Asm_raw.auipc ~rd ~imm20)));
  QCheck.( mk_test ~name:"addi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `addi (Asm_raw.addi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"slti" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `slti (Asm_raw.slti ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"sltiu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `sltiu (Asm_raw.sltiu ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"xori" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `xori (Asm_raw.xori ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"ori" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `ori (Asm_raw.ori ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"andi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `andi (Asm_raw.andi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"add" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `add (Asm_raw.add ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"sub" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `sub (Asm_raw.sub ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"sll" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `sll (Asm_raw.sll ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"slt" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `slt (Asm_raw.slt ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"sltu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `sltu (Asm_raw.sltu ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"xor" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `xor_ (Asm_raw.xor_ ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"srl" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `srl (Asm_raw.srl ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"sra" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `sra (Asm_raw.sra ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"or" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `or_ (Asm_raw.or_ ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"and" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, rs2) -> f `and_ (Asm_raw.and_ ~rd ~rs1 ~rs2)));
  QCheck.( mk_test ~name:"slli" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, shamtw) -> f `slli (Asm_raw.slli ~rd ~rs1 ~shamtw)));
  QCheck.( mk_test ~name:"srli" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, shamtw) -> f `srli (Asm_raw.srli ~rd ~rs1 ~shamtw)));
  QCheck.( mk_test ~name:"srai" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 32)) 
    (fun (rd, rs1, shamtw) -> f `srai (Asm_raw.srai ~rd ~rs1 ~shamtw)));
  QCheck.( mk_test ~name:"lb" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `lb (Asm_raw.lb ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"lh" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `lh (Asm_raw.lh ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"lw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `lw (Asm_raw.lw ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"lbu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `lbu (Asm_raw.lbu ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"lhu" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `lhu (Asm_raw.lhu ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"sb" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (imm12hi, rs1, rs2, imm12lo) -> f `sb (Asm_raw.sb ~imm12hi ~rs1 ~rs2 ~imm12lo)));
  QCheck.( mk_test ~name:"sh" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (imm12hi, rs1, rs2, imm12lo) -> f `sh (Asm_raw.sh ~imm12hi ~rs1 ~rs2 ~imm12lo)));
  QCheck.( mk_test ~name:"sw" ~n 
    ~pp:PP.(QCRV.PP.tuple4 int int int int) ~limit:2
    Arbitrary.(QCRV.tuple4 (int 128) (int 32) (int 32) (int 32)) 
    (fun (imm12hi, rs1, rs2, imm12lo) -> f `sw (Asm_raw.sw ~imm12hi ~rs1 ~rs2 ~imm12lo)));
  QCheck.( mk_test ~name:"fence" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 16) (int 16)) 
    (fun (pred, succ) -> f `fence (Asm_raw.fence ~pred ~succ)));
  QCheck.( mk_test ~name:"csrrw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrw (Asm_raw.csrrw ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrs" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrs (Asm_raw.csrrs ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrc" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrc (Asm_raw.csrrc ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrwi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrwi (Asm_raw.csrrwi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrsi" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrsi (Asm_raw.csrrsi ~rd ~rs1 ~imm12)));
  QCheck.( mk_test ~name:"csrrci" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 32) (int 32) (int 4096)) 
    (fun (rd, rs1, imm12) -> f `csrrci (Asm_raw.csrrci ~rd ~rs1 ~imm12)));
]

end

