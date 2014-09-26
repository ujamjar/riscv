open Types

exception RISCV_store_address_misaligned of int
exception RISCV_load_address_misaligned of int

let insert_bits m a d pos size = 
  let mask' = D.mask size in
  let a = a lsr 3 in
  m.{a} <- D.( (m.{a} &: (~: (mask' <<: pos))) |: ((d &: mask') <<: pos) )

let store_double m a d = 
  match a mod 8 with
  | 0 -> m.{ a lsr 3 } <- d
  | _ -> raise (RISCV_store_address_misaligned 64)

let store_word m a d = 
  match a mod 8 with
  | 0 -> insert_bits m a d 0 32
  | 4 -> insert_bits m a d 32 32
  | _ -> raise (RISCV_store_address_misaligned 32)

let store_half m a d = 
  match a mod 8 with
  | 0 -> insert_bits m a d 0 16
  | 2 -> insert_bits m a d 16 16
  | 4 -> insert_bits m a d 32 16
  | 6 -> insert_bits m a d 48 16
  | _ -> raise (RISCV_store_address_misaligned 16)

let store_byte m a d = 
  insert_bits m a d ((a mod 8)*8) 8

let load_double m a = 
  match a mod 8 with
  | 0 -> m.{a lsr 3}
  | _ -> raise (RISCV_load_address_misaligned 64)

let load_word = 
  let mask' = D.mask 32 in
  fun m a ->
    match a mod 8 with
    | 0 -> D.( (m.{a lsr 3} >>:  0) &: mask' )
    | 4 -> D.( (m.{a lsr 3} >>: 32) &: mask' )
    | _ -> raise (RISCV_load_address_misaligned 32)

let load_half = 
  let mask' = D.mask 16 in
  fun m a ->
    match a mod 8 with
    | 0 -> D.( (m.{a lsr 3} >>:  0) &: mask' )
    | 2 -> D.( (m.{a lsr 3} >>: 16) &: mask' )
    | 4 -> D.( (m.{a lsr 3} >>: 32) &: mask' )
    | 6 -> D.( (m.{a lsr 3} >>: 48) &: mask' )
    | _ -> raise (RISCV_load_address_misaligned 16)

let load_byte = 
  let mask' = D.mask 8 in
  fun m a ->
    let b = a mod 8 in
    D.( (m.{a lsr 3} >>: (b*8)) &: mask' )


