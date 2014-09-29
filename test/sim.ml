
module T = Types.Make(Types.D64)
module R = Instr.Make(T)

let riscv = T.riscv_init 1

let () = R.execute riscv (Types.I.of_int 0)


