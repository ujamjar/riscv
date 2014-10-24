exception RISCV_store_size_too_big
exception RISCV_load_size_too_big

exception RISCV_store_address_misaligned of int * int
exception RISCV_load_address_misaligned of int * int

module Make(T : Cpu.T) : sig

  open T

  val insert_bits : memory -> addr -> cpu_data -> int -> int -> unit

  (** store n memory addr data.  n=0,1,2,3 => 8,16,32,64 *)
  val store : int -> memory -> addr -> cpu_data -> unit

  (** load  n memory addr.  n=0,1,2,3 => 8,16,32,64 *)
  val load : int -> memory -> addr -> cpu_data 

end
