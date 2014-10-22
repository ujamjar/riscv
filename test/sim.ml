
module T = Types.Make(Types.D64)
module R = Instr.Make(T)

let riscv = T.riscv_init 1
let decode = Instr.Util.instruction_decoder RV64G.T.mask_match

let () = 
  let i = Types.I.of_int 0 in
  R.RV64G.exec riscv i (decode i)

