module type I = sig
  type t
  val bits : int
  val lbits : int
  val bytes : int
  val of_int : int -> t
  val to_int : t -> int 
  val of_float : float -> t
  val zero : t
  val one : t
  val ones : t
  val mask : int -> t
  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
  val ( *: ) : t -> t -> t
  val (&:) : t -> t -> t
  val (|:) : t -> t -> t
  val (^:) : t -> t -> t
  val (~:) : t -> t
  val (<<:) : t -> int -> t
  val (>>:) : t -> int -> t
  val (>>+) : t -> int -> t
  val (<:) : t -> t -> t
  val (<+) : t -> t -> t
  val (>=:) : t -> t -> t
  val (>=+) : t -> t -> t
  val extract_bits : pos:int -> size:int -> t -> t
end

(* XXX to make code generic between RV32 and RV64, 
   remove the with type annotations.  That will make
   various bits of code incompatible and we'll have to
   deal properly with things like constants *)
module I : I with type t = int32
(*module D : I with type t = int64*)
module D : sig
  include I
  val of_i : I.t -> t
end

type instr = I.t
type addr = int
type cpu_data = D.t
type regs = cpu_data array
type memory = (D.t, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

type priv = 
  {
    mutable fflags : D.t;
    mutable frm : D.t;
    mutable fcsr : D.t;
    mutable sup0 : D.t;
    mutable sup1 : D.t;
    mutable epc : D.t;
    mutable badvaddr : D.t;
    mutable ptbr : D.t;
    mutable asid : D.t;
    mutable count : D.t;
    mutable compare : D.t;
    mutable exec : D.t;
    mutable cause : D.t;
    mutable status : D.t; (* XXX actually 32 bit *)
    mutable hartid : D.t;
    mutable impl : D.t;
    mutable fatc : D.t;
    mutable send_ipi : D.t;
    mutable clear_ipi : D.t;
    mutable stats : D.t;
    mutable reset : D.t;
    mutable tohost : D.t;
    mutable fromhost : D.t;
    mutable cycle : D.t;
    mutable time : D.t;
    mutable instret : D.t;
  }

type riscv = 
  {
    mem : memory;
    regs : regs;
    priv : priv;
    mutable pc : D.t;
  }

(* status register mappings *)
module SR : sig
  val s : D.t
  val ps : D.t
  val ei : D.t
  val pei : D.t
  val ef : D.t
  val u64 : D.t
  val s64 : D.t
  val vm : D.t
  val ea : D.t
  val im : D.t
  val ip : D.t
  
  val set : D.t -> D.t
end

val riscv_init : int -> riscv

