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
end

module Asm = struct

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

