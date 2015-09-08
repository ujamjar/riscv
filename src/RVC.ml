module T = struct

type t = [
| `c_li
| `c_addi
| `c_addiw
| `c_ldsp
| `c_lwsp
| `c_sdsp
| `c_swsp
| `c_lw0
| `c_ld0
| `c_add
| `c_sub
| `c_move
| `c_j
| `c_ld
| `c_lw
| `c_sd
| `c_sw
| `c_beq
| `c_bne
| `c_flw
| `c_fld
| `c_fsw
| `c_fsd
| `c_slli
| `c_slli32
| `c_srli
| `c_srli32
| `c_srai
| `c_srai32
| `c_slliw
| `c_add3
| `c_sub3
| `c_or3
| `c_and3
]

let mask_match = [
  `c_li    , (0x0000001fl,0x00000000l);
  `c_addi  , (0x0000001fl,0x00000001l);
  `c_addiw , (0x0000001fl,0x0000001dl);
  `c_ldsp  , (0x0000001fl,0x00000004l);
  `c_lwsp  , (0x0000001fl,0x00000005l);
  `c_sdsp  , (0x0000001fl,0x00000006l);
  `c_swsp  , (0x0000001fl,0x00000008l);
  `c_lw0   , (0x0000801fl,0x00000012l);
  `c_ld0   , (0x0000801fl,0x00008012l);
  `c_add   , (0x0000801fl,0x0000001al);
  `c_sub   , (0x0000801fl,0x0000801al);
  `c_move  , (0x0000801fl,0x00000002l);
  `c_j     , (0x0000801fl,0x00008002l);
  `c_ld    , (0x0000001fl,0x00000009l);
  `c_lw    , (0x0000001fl,0x0000000al);
  `c_sd    , (0x0000001fl,0x0000000cl);
  `c_sw    , (0x0000001fl,0x0000000dl);
  `c_beq   , (0x0000001fl,0x00000010l);
  `c_bne   , (0x0000001fl,0x00000011l);
  `c_flw   , (0x0000001fl,0x00000014l);
  `c_fld   , (0x0000001fl,0x00000015l);
  `c_fsw   , (0x0000001fl,0x00000016l);
  `c_fsd   , (0x0000001fl,0x00000018l);
  `c_slli  , (0x00001c1fl,0x00000019l);
  `c_slli32, (0x00001c1fl,0x00000419l);
  `c_srli  , (0x00001c1fl,0x00000819l);
  `c_srli32, (0x00001c1fl,0x00000c19l);
  `c_srai  , (0x00001c1fl,0x00001019l);
  `c_srai32, (0x00001c1fl,0x00001419l);
  `c_slliw , (0x00001c1fl,0x00001819l);
  `c_add3  , (0x0000031fl,0x0000001cl);
  `c_sub3  , (0x0000031fl,0x0000011cl);
  `c_or3   , (0x0000031fl,0x0000021cl);
  `c_and3  , (0x0000031fl,0x0000031cl);
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
  | `c_li     ->
    ("c.li" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_addi   ->
    ("c.addi" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_addiw  ->
    ("c.addiw" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_ldsp   ->
    ("c.ldsp" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_lwsp   ->
    ("c.lwsp" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_sdsp   ->
    ("c.sdsp" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_swsp   ->
    ("c.swsp" ^ " cimm6=" ^ (x 15 10) ^ " crd=" ^ (x 9 5))
  | `c_lw0    ->
    ("c.lw0" ^ " crs1=" ^ (x 14 10) ^ " crd=" ^ (x 9 5))
  | `c_ld0    ->
    ("c.ld0" ^ " crs1=" ^ (x 14 10) ^ " crd=" ^ (x 9 5))
  | `c_add    ->
    ("c.add" ^ " crs1=" ^ (x 14 10) ^ " crd=" ^ (x 9 5))
  | `c_sub    ->
    ("c.sub" ^ " crs1=" ^ (x 14 10) ^ " crd=" ^ (x 9 5))
  | `c_move   ->
    ("c.move" ^ " crs1=" ^ (x 14 10) ^ " crd=" ^ (x 9 5))
  | `c_j      ->
    ("c.j" ^ " cimm10=" ^ (x 14 5))
  | `c_ld     ->
    ("c.ld" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_lw     ->
    ("c.lw" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_sd     ->
    ("c.sd" ^ " crs2s=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_sw     ->
    ("c.sw" ^ " crs2s=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_beq    ->
    ("c.beq" ^ " crs2s=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_bne    ->
    ("c.bne" ^ " crs2s=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_flw    ->
    ("c.flw" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_fld    ->
    ("c.fld" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_fsw    ->
    ("c.fsw" ^ " crs2s=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_fsd    ->
    ("c.fsd" ^ " crs2s=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " cimm5=" ^ (x 9 5))
  | `c_slli   ->
    ("c.slli" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_slli32 ->
    ("c.slli32" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_srli   ->
    ("c.srli" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_srli32 ->
    ("c.srli32" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_srai   ->
    ("c.srai" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_srai32 ->
    ("c.srai32" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_slliw  ->
    ("c.slliw" ^ " crds=" ^ (x 15 13) ^ " cimm5=" ^ (x 9 5))
  | `c_add3   ->
    ("c.add3" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " crs2bs=" ^ (x 7 5))
  | `c_sub3   ->
    ("c.sub3" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " crs2bs=" ^ (x 7 5))
  | `c_or3    ->
    ("c.or3" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " crs2bs=" ^ (x 7 5))
  | `c_and3   ->
    ("c.and3" ^ " crds=" ^ (x 15 13) ^ " crs1s=" ^ (x 12 10) ^ " crs2bs=" ^ (x 7 5))
let fields =
  let open Types.Fields in
  [
    (`c_li, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(0)); ]);
    (`c_addi, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(1)); ]);
    (`c_addiw, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(29)); ]);
    (`c_ldsp, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(4)); ]);
    (`c_lwsp, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(5)); ]);
    (`c_sdsp, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(6)); ]);
    (`c_swsp, [ Field((`cimm6,"cimm6",(15,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(8)); ]);
    (`c_lw0, [ Bit(15,Int(0)); Field((`crs1,"crs1",(14,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(18)); ]);
    (`c_ld0, [ Bit(15,Int(1)); Field((`crs1,"crs1",(14,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(18)); ]);
    (`c_add, [ Bit(15,Int(0)); Field((`crs1,"crs1",(14,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(26)); ]);
    (`c_sub, [ Bit(15,Int(1)); Field((`crs1,"crs1",(14,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(26)); ]);
    (`c_move, [ Bit(15,Int(0)); Field((`crs1,"crs1",(14,10)), Nothing); Field((`crd,"crd",(9,5)), Nothing); Range((4,0),Int(2)); ]);
    (`c_j, [ Bit(15,Int(1)); Field((`cimm10,"cimm10",(14,5)), Nothing); Range((4,0),Int(2)); ]);
    (`c_ld, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(9)); ]);
    (`c_lw, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(10)); ]);
    (`c_sd, [ Field((`crs2s,"crs2s",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(12)); ]);
    (`c_sw, [ Field((`crs2s,"crs2s",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(13)); ]);
    (`c_beq, [ Field((`crs2s,"crs2s",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(16)); ]);
    (`c_bne, [ Field((`crs2s,"crs2s",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(17)); ]);
    (`c_flw, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(20)); ]);
    (`c_fld, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(21)); ]);
    (`c_fsw, [ Field((`crs2s,"crs2s",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(22)); ]);
    (`c_fsd, [ Field((`crs2s,"crs2s",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(24)); ]);
    (`c_slli, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(0)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_slli32, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(1)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_srli, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(2)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_srli32, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(3)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_srai, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(4)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_srai32, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(5)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_slliw, [ Field((`crds,"crds",(15,13)), Nothing); Range((12,10),Int(6)); Field((`cimm5,"cimm5",(9,5)), Nothing); Range((4,0),Int(25)); ]);
    (`c_add3, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Range((9,8),Int(0)); Field((`crs2bs,"crs2bs",(7,5)), Nothing); Range((4,0),Int(28)); ]);
    (`c_sub3, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Range((9,8),Int(1)); Field((`crs2bs,"crs2bs",(7,5)), Nothing); Range((4,0),Int(28)); ]);
    (`c_or3, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Range((9,8),Int(2)); Field((`crs2bs,"crs2bs",(7,5)), Nothing); Range((4,0),Int(28)); ]);
    (`c_and3, [ Field((`crds,"crds",(15,13)), Nothing); Field((`crs1s,"crs1s",(12,10)), Nothing); Range((9,8),Int(3)); Field((`crs2bs,"crs2bs",(7,5)), Nothing); Range((4,0),Int(28)); ]);
  ]

end

module Asm_raw = struct

let c_li ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x0l)

let c_addi ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x1l)

let c_addiw ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x1dl)

let c_ldsp ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x4l)

let c_lwsp ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x5l)

let c_sdsp ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x6l)

let c_swsp ~cimm6 ~crd = Types.I.(
  (((of_int cimm6) &: 0x3fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x8l)

let c_lw0 ~crs1 ~crd = Types.I.(
  (((of_int crs1) &: 0x1fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x12l)

let c_ld0 ~crs1 ~crd = Types.I.(
  (((of_int crs1) &: 0x1fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x8012l)

let c_add ~crs1 ~crd = Types.I.(
  (((of_int crs1) &: 0x1fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x1al)

let c_sub ~crs1 ~crd = Types.I.(
  (((of_int crs1) &: 0x1fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x801al)

let c_move ~crs1 ~crd = Types.I.(
  (((of_int crs1) &: 0x1fl) <<: 10) |:
  (((of_int crd) &: 0x1fl) <<: 5) |:
  0x2l)

let c_j ~cimm10 = Types.I.(
  (((of_int cimm10) &: 0x3ffl) <<: 5) |:
  0x8002l)

let c_ld ~crds ~crs1s ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x9l)

let c_lw ~crds ~crs1s ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0xal)

let c_sd ~crs2s ~crs1s ~cimm5 = Types.I.(
  (((of_int crs2s) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0xcl)

let c_sw ~crs2s ~crs1s ~cimm5 = Types.I.(
  (((of_int crs2s) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0xdl)

let c_beq ~crs2s ~crs1s ~cimm5 = Types.I.(
  (((of_int crs2s) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x10l)

let c_bne ~crs2s ~crs1s ~cimm5 = Types.I.(
  (((of_int crs2s) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x11l)

let c_flw ~crds ~crs1s ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x14l)

let c_fld ~crds ~crs1s ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x15l)

let c_fsw ~crs2s ~crs1s ~cimm5 = Types.I.(
  (((of_int crs2s) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x16l)

let c_fsd ~crs2s ~crs1s ~cimm5 = Types.I.(
  (((of_int crs2s) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x18l)

let c_slli ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x19l)

let c_slli32 ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x419l)

let c_srli ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x819l)

let c_srli32 ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0xc19l)

let c_srai ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x1019l)

let c_srai32 ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x1419l)

let c_slliw ~crds ~cimm5 = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int cimm5) &: 0x1fl) <<: 5) |:
  0x1819l)

let c_add3 ~crds ~crs1s ~crs2bs = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int crs2bs) &: 0x7l) <<: 5) |:
  0x1cl)

let c_sub3 ~crds ~crs1s ~crs2bs = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int crs2bs) &: 0x7l) <<: 5) |:
  0x11cl)

let c_or3 ~crds ~crs1s ~crs2bs = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int crs2bs) &: 0x7l) <<: 5) |:
  0x21cl)

let c_and3 ~crds ~crs1s ~crs2bs = Types.I.(
  (((of_int crds) &: 0x7l) <<: 13) |:
  (((of_int crs1s) &: 0x7l) <<: 10) |:
  (((of_int crs2bs) &: 0x7l) <<: 5) |:
  0x31cl)

end

module Asm = struct

let c_li = Asm_raw.c_li
let c_addi = Asm_raw.c_addi
let c_addiw = Asm_raw.c_addiw
let c_ldsp = Asm_raw.c_ldsp
let c_lwsp = Asm_raw.c_lwsp
let c_sdsp = Asm_raw.c_sdsp
let c_swsp = Asm_raw.c_swsp
let c_lw0 = Asm_raw.c_lw0
let c_ld0 = Asm_raw.c_ld0
let c_add = Asm_raw.c_add
let c_sub = Asm_raw.c_sub
let c_move = Asm_raw.c_move
let c_j = Asm_raw.c_j
let c_ld = Asm_raw.c_ld
let c_lw = Asm_raw.c_lw
let c_sd = Asm_raw.c_sd
let c_sw = Asm_raw.c_sw
let c_beq = Asm_raw.c_beq
let c_bne = Asm_raw.c_bne
let c_flw = Asm_raw.c_flw
let c_fld = Asm_raw.c_fld
let c_fsw = Asm_raw.c_fsw
let c_fsd = Asm_raw.c_fsd
let c_slli = Asm_raw.c_slli
let c_slli32 = Asm_raw.c_slli32
let c_srli = Asm_raw.c_srli
let c_srli32 = Asm_raw.c_srli32
let c_srai = Asm_raw.c_srai
let c_srai32 = Asm_raw.c_srai32
let c_slliw = Asm_raw.c_slliw
let c_add3 = Asm_raw.c_add3
let c_sub3 = Asm_raw.c_sub3
let c_or3 = Asm_raw.c_or3
let c_and3 = Asm_raw.c_and3
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"c.li" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_li (Asm_raw.c_li ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.addi" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_addi (Asm_raw.c_addi ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.addiw" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_addiw (Asm_raw.c_addiw ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.ldsp" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_ldsp (Asm_raw.c_ldsp ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.lwsp" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_lwsp (Asm_raw.c_lwsp ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.sdsp" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_sdsp (Asm_raw.c_sdsp ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.swsp" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 64) (int 32)) 
    (fun (cimm6, crd) -> f `c_swsp (Asm_raw.c_swsp ~cimm6 ~crd)));
  QCheck.( mk_test ~name:"c.lw0" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (crs1, crd) -> f `c_lw0 (Asm_raw.c_lw0 ~crs1 ~crd)));
  QCheck.( mk_test ~name:"c.ld0" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (crs1, crd) -> f `c_ld0 (Asm_raw.c_ld0 ~crs1 ~crd)));
  QCheck.( mk_test ~name:"c.add" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (crs1, crd) -> f `c_add (Asm_raw.c_add ~crs1 ~crd)));
  QCheck.( mk_test ~name:"c.sub" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (crs1, crd) -> f `c_sub (Asm_raw.c_sub ~crs1 ~crd)));
  QCheck.( mk_test ~name:"c.move" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 32) (int 32)) 
    (fun (crs1, crd) -> f `c_move (Asm_raw.c_move ~crs1 ~crd)));
  QCheck.( mk_test ~name:"c.j" ~n 
    ~pp:PP.(QCRV.PP.tuple1 int) ~limit:2
    Arbitrary.(QCRV.tuple1 (int 1024)) 
    (fun (cimm10) -> f `c_j (Asm_raw.c_j ~cimm10)));
  QCheck.( mk_test ~name:"c.ld" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crds, crs1s, cimm5) -> f `c_ld (Asm_raw.c_ld ~crds ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.lw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crds, crs1s, cimm5) -> f `c_lw (Asm_raw.c_lw ~crds ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.sd" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crs2s, crs1s, cimm5) -> f `c_sd (Asm_raw.c_sd ~crs2s ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.sw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crs2s, crs1s, cimm5) -> f `c_sw (Asm_raw.c_sw ~crs2s ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.beq" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crs2s, crs1s, cimm5) -> f `c_beq (Asm_raw.c_beq ~crs2s ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.bne" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crs2s, crs1s, cimm5) -> f `c_bne (Asm_raw.c_bne ~crs2s ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.flw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crds, crs1s, cimm5) -> f `c_flw (Asm_raw.c_flw ~crds ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.fld" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crds, crs1s, cimm5) -> f `c_fld (Asm_raw.c_fld ~crds ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.fsw" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crs2s, crs1s, cimm5) -> f `c_fsw (Asm_raw.c_fsw ~crs2s ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.fsd" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 32)) 
    (fun (crs2s, crs1s, cimm5) -> f `c_fsd (Asm_raw.c_fsd ~crs2s ~crs1s ~cimm5)));
  QCheck.( mk_test ~name:"c.slli" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_slli (Asm_raw.c_slli ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.slli32" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_slli32 (Asm_raw.c_slli32 ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.srli" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_srli (Asm_raw.c_srli ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.srli32" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_srli32 (Asm_raw.c_srli32 ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.srai" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_srai (Asm_raw.c_srai ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.srai32" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_srai32 (Asm_raw.c_srai32 ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.slliw" ~n 
    ~pp:PP.(QCRV.PP.tuple2 int int) ~limit:2
    Arbitrary.(QCRV.tuple2 (int 8) (int 32)) 
    (fun (crds, cimm5) -> f `c_slliw (Asm_raw.c_slliw ~crds ~cimm5)));
  QCheck.( mk_test ~name:"c.add3" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 8)) 
    (fun (crds, crs1s, crs2bs) -> f `c_add3 (Asm_raw.c_add3 ~crds ~crs1s ~crs2bs)));
  QCheck.( mk_test ~name:"c.sub3" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 8)) 
    (fun (crds, crs1s, crs2bs) -> f `c_sub3 (Asm_raw.c_sub3 ~crds ~crs1s ~crs2bs)));
  QCheck.( mk_test ~name:"c.or3" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 8)) 
    (fun (crds, crs1s, crs2bs) -> f `c_or3 (Asm_raw.c_or3 ~crds ~crs1s ~crs2bs)));
  QCheck.( mk_test ~name:"c.and3" ~n 
    ~pp:PP.(QCRV.PP.tuple3 int int int) ~limit:2
    Arbitrary.(QCRV.tuple3 (int 8) (int 8) (int 8)) 
    (fun (crds, crs1s, crs2bs) -> f `c_and3 (Asm_raw.c_and3 ~crds ~crs1s ~crs2bs)));
]

end

