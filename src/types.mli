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
  val to_string : t -> string
  val to_hex : t -> string
end

module I : I with type t = int32

module type D = sig
  include I
  val of_i : I.t -> t
  val to_i : t -> I.t 
end

module Fields : sig
  type field = [
    | `rd | `rs1 | `rs2 | `rs3
    | `aqrl | `pred | `succ | `rm
    | `imm20 | `jimm20 | `imm12 | `imm12hi
    | `bimm12hi | `imm12lo | `bimm12lo | `zimm
    | `shamt | `shamtw | `vseglen
    | `crd | `crs2 | `crs1 | `crds | `crs2s
    | `crs2bs | `crs1s | `cimm6 | `cimm10 | `cimm5
  ]
  val fields : (field * string * (int * int)) list
  type value =
    | Int of int
    | Ignore
    | Nothing
  type t = 
    | Field of (field * string * (int * int)) * value
    | Bit of int * value
    | Range of (int * int) * value
end

module D32 : D with type t = int32
module D64 : D with type t = int64

val abi_name_of_reg : int -> string
val abi_name_of_freg : int -> string



