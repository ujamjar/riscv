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
  val sll : t -> int -> t
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
  let sll = Int32.shift_left
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
    (v >>: pos) &: ((sll one size) -: one)
  let mask n = if n=bits then ones else (sll one n) -: one
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
  let sll = Int64.shift_left
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
    (v >>: pos) &: ((sll one size) -: one)
  let mask n = if n=bits then ones else (sll one n) -: one
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

module Fields = struct
  type field = [
    | `rd      
    | `rs1     
    | `rs2     
    | `rs3     
    | `aqrl    
    | `pred    
    | `succ    
    | `rm      
    | `imm20   
    | `jimm20  
    | `imm12   
    | `imm12hi 
    | `bimm12hi
    | `imm12lo 
    | `bimm12lo
    | `zimm    
    | `shamt   
    | `shamtw  
    | `vseglen 
    | `crd     
    | `crs2    
    | `crs1    
    | `crds    
    | `crs2s   
    | `crs2bs  
    | `crs1s   
    | `cimm6   
    | `cimm10  
    | `cimm5   
  ]
  let fields : (field * string * (int * int)) list = [
    (* 32 bit instructions *)
    `rd      , "rd"      , (11, 7);
    `rs1     , "rs1"     , (19,15);
    `rs2     , "rs2"     , (24,20);
    `rs3     , "rs3"     , (31,27);
    `aqrl    , "aqrl"    , (26,25);
    `pred    , "pred"    , (27,24);
    `succ    , "succ"    , (23,20);
    `rm      , "rm"      , (14,12);
    `imm20   , "imm20"   , (31,12);
    `jimm20  , "jimm20"  , (31,12);
    `imm12   , "imm12"   , (31,20);
    `imm12hi , "imm12hi" , (31,25);
    `bimm12hi, "bimm12hi", (31,25);
    `imm12lo , "imm12lo" , (11, 7);
    `bimm12lo, "bimm12lo", (11, 7);
    `zimm    , "zimm"    , (19,15);
    `shamt   , "shamt"   , (25,20);
    `shamtw  , "shamtw"  , (24,20);
    `vseglen , "vseglen" , (31,29);
    (* 16 bit compressed instructions *)
    `crd     , "crd"     , ( 9, 5);
    `crs2    , "crs2"    , ( 9, 5);
    `crs1    , "crs1"    , (14,10);
    `crds    , "crds"    , (15,13);
    `crs2s   , "crs2s"   , (15,13);
    `crs2bs  , "crs2bs"  , ( 7, 5);
    `crs1s   , "crs1s"   , (12,10);
    `cimm6   , "cimm6"   , (15,10);
    `cimm10  , "cimm10"  , (14, 5);
    `cimm5   , "cimm5"   , ( 9, 5);
  ]
  type value =
    | Int of int
    | Ignore
    | Nothing
  type t = 
    | Field of (field * string * (int * int)) * value
    | Bit of int * value
    | Range of (int * int) * value
end

module D64 = struct
  include I64
  let of_i = Int64.of_int32
end

module D32 = struct
  include I32
  let of_i x = x
end

let abi_name_of_reg r = 
  if r = 0 then "zero"
  else if r = 1 then "ra"
  else if r = 2 then "fp"
  else if r >= 3 && r <= 13 then "s" ^ string_of_int (r-2)
  else if r = 14 then "sp"
  else if r = 15 then "tp"
  else if r >= 16 && r <= 17 then "v" ^ string_of_int (r-16)
  else if r >= 18 && r <= 25 then "a" ^ string_of_int (r-18)
  else if r >= 26 && r <= 30 then "t" ^ string_of_int (r-26)
  else if r = 31 then "gp"
  else raise Not_found

let abi_name_of_freg r = 
  if r >= 0 && r <= 15 then "fs" ^ string_of_int r
  else if r >= 16 && r <= 17 then "rv" ^ string_of_int (r-16)
  else if r >= 18 && r <= 25 then "ra" ^ string_of_int (r-18)
  else if r >= 26 && r <= 31 then "rt" ^ string_of_int (r-26)
  else raise Not_found

