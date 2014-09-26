open Types

let riscv = riscv_init 1

let x = Mem.store_word
let () = Instr.execute riscv (I.of_int 0)


