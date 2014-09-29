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

module I32 = struct
  type t = int32
  let bytes = 4
  let lbytes = 2
  let bits = 32
  let lbits = 5
  let of_int = Int32.of_int
  let to_int = Int32.to_int
  let of_float = Int32.of_float
  let zero = 0l
  let one = 1l
  let ones = -1l
  let (+:) = Int32.add
  let (-:) = Int32.sub
  let ( *: ) = Int32.mul
  let (&:) = Int32.logand
  let (|:) = Int32.logor
  let (^:) = Int32.logxor
  let (~:) = Int32.lognot
  let (<<:) = Int32.shift_left
  let (>>:) = Int32.shift_right_logical
  let (>>+) = Int32.shift_right
  let (<+) a b = if Int32.compare a b = (-1) then 1l else 0l
  let (<:) a b = 
    let a' = Int32.(shift_right_logical a 31) in
    let b' = Int32.(shift_right_logical b 31) in
    if a' < b' then 1l
    else if a' > b' then 0l
    else if Int32.( (logand a 0x7fff_ffffl) < (logand b 0x7fff_ffffl) ) then 1l
    else 0l
  let (>=+) a b = if Int32.compare a b >= 0 then 1l else 0l
  let (>=:) a b = 
    let a' = Int32.(shift_right_logical a 31) in
    let b' = Int32.(shift_right_logical b 31) in
    if a' > b' then 1l
    else if a' < b' then 0l
    else if Int32.( (logand a 0x7fff_ffffl) >= (logand b 0x7fff_ffffl) ) then 1l
    else 0l
  let extract_bits ~pos ~size v = 
    (v >>: pos) &: ((one <<: size) -: one)
  let mask n = if n=bits then ones else (one <<: n) -: one
  module A = struct
    type arr = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
    let create len init = 
      let arr = Bigarray.(Array1.create int32 c_layout len) in
      for i=0 to len-1 do
        arr.{i} <- init
      done;
      arr
    let length = Bigarray.Array1.dim
    let get arr n = arr.{n}
    let set arr n v = arr.{n} <- v
  end
end

module I64 = struct
  type t = int64
  let bytes = 8
  let lbytes = 3
  let bits = 64
  let lbits = 6
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let of_float = Int64.of_float
  let zero = 0L
  let one = 1L
  let ones = -1L
  let (+:) = Int64.add
  let (-:) = Int64.sub
  let ( *: ) = Int64.mul
  let (&:) = Int64.logand
  let (|:) = Int64.logor
  let (^:) = Int64.logxor
  let (~:) = Int64.lognot
  let (<<:) = Int64.shift_left
  let (>>:) = Int64.shift_right_logical
  let (>>+) = Int64.shift_right
  let (<+) a b = if Int64.compare a b = (-1) then 1L else 0L
  let (<:) a b = 
    let a' = Int64.(shift_right_logical a 63) in
    let b' = Int64.(shift_right_logical b 63) in
    if a' < b' then 1L
    else if a' > b' then 0L
    else if Int64.( (logand a 0x7fff_ffff_ffff_ffffL) < (logand b 0x7fff_ffff_ffff_ffffL) ) then 1L
    else 0L
  let (>=+) a b = if Int64.compare a b >= 0 then 1L else 0L
  let (>=:) a b = 
    let a' = Int64.(shift_right_logical a 63) in
    let b' = Int64.(shift_right_logical b 63) in
    if a' > b' then 1L
    else if a' < b' then 0L
    else if Int64.( (logand a 0x7fff_ffff_ffff_ffffL) >= (logand b 0x7fff_ffff_ffff_ffffL) ) then 1L
    else 0L
  let extract_bits ~pos ~size v = 
    (v >>: pos) &: ((one <<: size) -: one)
  let mask n = if n=bits then ones else (one <<: n) -: one
  module A = struct
    type arr = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
    let create len init = 
      let arr = Bigarray.(Array1.create int64 c_layout len) in
      for i=0 to len-1 do
        arr.{i} <- init
      done;
      arr
    let length = Bigarray.Array1.dim
    let get arr n = arr.{n}
    let set arr n v = arr.{n} <- v
  end
end

module I = I32

module type D = sig
  include I
  val of_i : I.t -> t
end

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
      mutable time : cpu_data;
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

module Make(D : D) = struct

  module D = D
    
  type instr = I.t
  type addr = int
  type cpu_data = D.t
  type regs = cpu_data array
  type memory = D.A.arr

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

  module SR = struct
    let s   = D.(one <<: 0)
    let ps  = D.(one <<: 1)
    let ei  = D.(one <<: 2)
    let pei = D.(one <<: 3)
    let ef  = D.(one <<: 4)
    let u64 = D.(one <<: 5)
    let s64 = D.(one <<: 6)
    let vm  = D.(one <<: 7)
    let ea  = D.(one <<: 8)
    let im  = D.(mask 8 <<: 16)
    let ip  = D.(mask 8 <<: 24)
    
    (* set status reg value.  u64+s64 on, ef off *)
    let set v = D.(v |: u64 |: s64 &: (~: ef))
  end

  let riscv_init memsize_mb = 
    let words = memsize_mb * 1024 / D.bytes in
    let mem = D.A.create words D.zero in
    let priv = 
      {
        fflags = D.zero;
        frm = D.zero;
        fcsr = D.zero;
        sup0 = D.zero;
        sup1 = D.zero;
        epc = D.zero;
        badvaddr = D.zero;
        ptbr = D.zero;
        asid = D.zero;
        count = D.zero;
        compare = D.zero;
        exec = D.zero;
        cause = D.zero;
        status = SR.(set s); 
        hartid = D.zero;
        impl = D.zero;
        fatc = D.zero;
        send_ipi = D.zero;
        clear_ipi = D.zero;
        stats = D.zero;
        reset = D.zero;
        tohost = D.zero;
        fromhost = D.zero;
        cycle = D.zero;
        time = D.of_float (Unix.gettimeofday () *. 1000.); (* roughly, system boot time *)
        instret = D.zero;
      }
    in
    {
      mem = mem;
      regs = Array.init 32 (fun _ -> D.zero);
      priv = priv;
      pc = D.zero;
    }

end


module D64 = struct
  include I64
  let of_i = Int64.of_int32
end

module D32 = struct
  include I32
  let of_i x = x
end

