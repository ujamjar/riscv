module type I = sig
  type t
  val bits : int
  val lbits : int
  val bytes : int
  val lbytes : int
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
  module A : sig
    type arr
    val create : int -> t -> arr
    val length : arr -> int
    val get : arr -> int -> t
    val set : arr -> int -> t -> unit
  end
end

module I : I with type t = int32

module type D = sig
  include I
  val of_i : I.t -> t
end

module D32 : D with type t = int32
module D64 : D with type t = int64

module type T = sig

  module D : D

  type instr = I.t
  type addr = int
  type cpu_data = D.t
  type regs = cpu_data array
  type memory = D.A.arr

  type priv = 
    {
      mutable fflags : cpu_data;
      mutable frm : cpu_data;
      mutable fcsr : cpu_data;
      mutable sup0 : cpu_data;
      mutable sup1 : cpu_data;
      mutable epc : cpu_data;
      mutable badvaddr : cpu_data;
      mutable ptbr : cpu_data;
      mutable asid : cpu_data;
      mutable count : cpu_data;
      mutable compare : cpu_data;
      mutable exec : cpu_data;
      mutable cause : cpu_data;
      mutable status : cpu_data; (* XXX actually 32 bit *)
      mutable hartid : cpu_data;
      mutable impl : cpu_data;
      mutable fatc : cpu_data;
      mutable send_ipi : cpu_data;
      mutable clear_ipi : cpu_data;
      mutable stats : cpu_data;
      mutable reset : cpu_data;
      mutable tohost : cpu_data;
      mutable fromhost : cpu_data;
      mutable cycle : cpu_data;
      mutable time : cpu_data; (* XXX some of these must be 64 bit, regardless of cpu width *)
      mutable instret : cpu_data;
    }

  type riscv = 
    {
      mem : memory;
      regs : regs;
      priv : priv;
      mutable pc : cpu_data;
    }

  (* status register mappings *)
  module SR : sig
    val s : cpu_data
    val ps : cpu_data
    val ei : cpu_data
    val pei : cpu_data
    val ef : cpu_data
    val u64 : cpu_data
    val s64 : cpu_data
    val vm : cpu_data
    val ea : cpu_data
    val im : cpu_data
    val ip : cpu_data
    
    val set : cpu_data -> cpu_data
  end

  val riscv_init : int -> riscv

end

module Make(D : D) : T with type D.t = D.t

