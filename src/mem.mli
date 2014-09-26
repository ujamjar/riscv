open Types

exception RISCV_store_address_misaligned of int
exception RISCV_load_address_misaligned of int

val insert_bits : memory -> addr -> cpu_data -> int -> int -> unit

val store_double : memory -> addr -> cpu_data -> unit
val store_word : memory -> addr -> cpu_data -> unit
val store_half : memory -> addr -> cpu_data -> unit
val store_byte : memory -> addr -> cpu_data -> unit

val load_double : memory -> addr -> cpu_data 
val load_word : memory -> addr -> cpu_data 
val load_half : memory -> addr -> cpu_data 
val load_byte : memory -> addr -> cpu_data 

