let msk x n = x land ((1 lsl n)-1)

let i_imm f ~imm = 
  let imm12 = msk imm 12 in
  f ~imm12

let sh_imm f ~imm = 
  let shamt = msk imm 6 in
  f ~shamt

let shw_imm f ~imm = 
  let shamtw = msk imm 5 in
  f ~shamtw

let s_imm f ~imm =
  let imm12lo = msk imm 5 in
  let imm12hi = msk (imm lsr 5) 7 in
  f ~imm12hi ~imm12lo

let b_imm f ~imm = 
  let bimm12lo = ((msk (imm lsr 1) 4) lsl 1) lor (msk (imm lsr 11) 1) in
  let bimm12hi = (msk (imm lsr 5) 6) lor ((msk (imm lsr 12) 1) lsl 6) in
  f ~bimm12hi ~bimm12lo

let u_imm f ~imm = 
  let imm20 = msk (imm lsr 12) 20 in (* XXX wouldn't get full range in 32 bit mode... *)
  f ~imm20

let j_imm f ~imm = 
  let jimm20 = 
    ((msk (imm lsr 12) 3) lsl  0) lor
    ((msk (imm lsr 15) 5) lsl  3) lor
    ((msk (imm lsr 11) 1) lsl  8) lor
    ((msk (imm lsr  1) 4) lsl  9) lor
    ((msk (imm lsr  5) 6) lsl 13) lor
    ((msk (imm lsr 20) 1) lsl 19)
  in
  f ~jimm20



