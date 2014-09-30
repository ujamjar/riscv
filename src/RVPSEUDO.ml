module T = struct

type t = [
| `_slli_rv32
| `_srli_rv32
| `_srai_rv32
| `_frflags
| `_fsflags
| `_fsflagsi
| `_frrm
| `_fsrm
| `_fsrmi
| `_fscsr
| `_frcsr
| `_rdcycle
| `_rdtime
| `_rdinstret
| `_rdcycleh
| `_rdtimeh
| `_rdinstreth
]

let mask_match = [
  `_slli_rv32, (0xfe00707fl,0x00001013l);
  `_srli_rv32, (0xfe00707fl,0x00005013l);
  `_srai_rv32, (0xfe00707fl,0x40005013l);
  `_frflags, (0xfffff07fl,0x00102073l);
  `_fsflags, (0xfff0707fl,0x00101073l);
  `_fsflagsi, (0xfff0707fl,0x00105073l);
  `_frrm   , (0xfffff07fl,0x00202073l);
  `_fsrm   , (0xfff0707fl,0x00201073l);
  `_fsrmi  , (0xfff0707fl,0x00205073l);
  `_fscsr  , (0xfff0707fl,0x00301073l);
  `_frcsr  , (0xfffff07fl,0x00302073l);
  `_rdcycle, (0xfffff07fl,0xc0002073l);
  `_rdtime , (0xfffff07fl,0xc0102073l);
  `_rdinstret, (0xfffff07fl,0xc0202073l);
  `_rdcycleh, (0xfffff07fl,0xc8002073l);
  `_rdtimeh, (0xfffff07fl,0xc8102073l);
  `_rdinstreth, (0xfffff07fl,0xc8202073l);
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
  | `_slli_rv32 ->
    ("@slli.rv32" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `_srli_rv32 ->
    ("@srli.rv32" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `_srai_rv32 ->
    ("@srai.rv32" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15) ^ " shamtw=" ^ (x 24 20))
  | `_frflags ->
    ("@frflags" ^ " rd=" ^ (x 11 7))
  | `_fsflags ->
    ("@fsflags" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `_fsflagsi ->
    ("@fsflagsi" ^ " rd=" ^ (x 11 7) ^ " zimm=" ^ (x 19 15))
  | `_frrm    ->
    ("@frrm" ^ " rd=" ^ (x 11 7))
  | `_fsrm    ->
    ("@fsrm" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `_fsrmi   ->
    ("@fsrmi" ^ " rd=" ^ (x 11 7) ^ " zimm=" ^ (x 19 15))
  | `_fscsr   ->
    ("@fscsr" ^ " rd=" ^ (x 11 7) ^ " rs1=" ^ (x 19 15))
  | `_frcsr   ->
    ("@frcsr" ^ " rd=" ^ (x 11 7))
  | `_rdcycle ->
    ("@rdcycle" ^ " rd=" ^ (x 11 7))
  | `_rdtime  ->
    ("@rdtime" ^ " rd=" ^ (x 11 7))
  | `_rdinstret ->
    ("@rdinstret" ^ " rd=" ^ (x 11 7))
  | `_rdcycleh ->
    ("@rdcycleh" ^ " rd=" ^ (x 11 7))
  | `_rdtimeh ->
    ("@rdtimeh" ^ " rd=" ^ (x 11 7))
  | `_rdinstreth ->
    ("@rdinstreth" ^ " rd=" ^ (x 11 7))
end

module Asm = struct

let _slli_rv32 ~rd ~rs1 ~shamtw = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamtw) &: 0x1fl) <<: 20) |:
  0x1013l)

let _srli_rv32 ~rd ~rs1 ~shamtw = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamtw) &: 0x1fl) <<: 20) |:
  0x5013l)

let _srai_rv32 ~rd ~rs1 ~shamtw = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  (((of_int shamtw) &: 0x1fl) <<: 20) |:
  0x40005013l)

let _frflags ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0x102073l)

let _fsflags ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0x101073l)

let _fsflagsi ~rd ~zimm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int zimm) &: 0x1fl) <<: 15) |:
  0x105073l)

let _frrm ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0x202073l)

let _fsrm ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0x201073l)

let _fsrmi ~rd ~zimm = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int zimm) &: 0x1fl) <<: 15) |:
  0x205073l)

let _fscsr ~rd ~rs1 = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  (((of_int rs1) &: 0x1fl) <<: 15) |:
  0x301073l)

let _frcsr ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0x302073l)

let _rdcycle ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0xc0002073l)

let _rdtime ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0xc0102073l)

let _rdinstret ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0xc0202073l)

let _rdcycleh ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0xc8002073l)

let _rdtimeh ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0xc8102073l)

let _rdinstreth ~rd = Types.I.(
  (((of_int rd) &: 0x1fl) <<: 7) |:
  0xc8202073l)

end

