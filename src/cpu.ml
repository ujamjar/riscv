open Types

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
    let s   = D.(sll one 0)
    let ps  = D.(sll one 1)
    let ei  = D.(sll one 2)
    let pei = D.(sll one 3)
    let ef  = D.(sll one 4)
    let u64 = D.(sll one 5)
    let s64 = D.(sll one 6)
    let vm  = D.(sll one 7)
    let ea  = D.(sll one 8)
    let im  = D.(sll (mask 8) 16)
    let ip  = D.(sll (mask 8) 24)
    
    (* set status reg value.  u64+s64 on, ef off *)
    let set v = D.(v |: u64 |: s64 &: (~: ef))
  end

  let riscv_init memsize_mb = 
    let words = memsize_mb * 1024 * 1024 / D.bytes in
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

