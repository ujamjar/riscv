module Make(T : Cpu.T) : sig

  module String_map : Map.S with type key = string

  type sym32 = 
    {
      st32_name : Int32.t;
      st32_value : Int32.t;
      st32_size : Int32.t;
      st32_info : int; (* 8 *)
      st32_other : int; (* 8 *)
      st32_shndx : int; (* 16 *)
    }
  type sym64 = 
    {
      st64_name : Int32.t;
      st64_info: int (* 8 *);
      st64_other: int (* 8 *);
      st64_shndx: int (* 16 *);
      st64_value : Int64.t;
      st64_size : Int64.t;
    }

  val to_mem : string -> T.memory -> sym64 String_map.t

end

