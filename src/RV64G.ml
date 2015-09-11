module T = struct

  type t = [
    | RV64I.T.t
    | RV64M.T.t
    | RV64A.T.t
    | RV64F.T.t
    | RV64D.T.t
    | RV32M.T.t
    | RV32A.T.t
    | RV32F.T.t
    | RV32D.T.t
    | RVSYS.T.t
  ]

  let mask_match = 
    List.concat
      [
        (RV64I.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64M.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64A.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64F.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64D.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32M.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32A.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32F.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32D.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RVSYS.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
      ]

  let to_t i = 
    let rec f = function
      | [] -> raise Not_found
      | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
    in
    f mask_match

  let pretty i = 
    try RV64I.T.pretty i with Not_found -> 
    try RV64M.T.pretty i with Not_found -> 
    try RV64A.T.pretty i with Not_found -> 
    try RV64F.T.pretty i with Not_found -> 
    try RV64D.T.pretty i with Not_found -> 
    try RV32M.T.pretty i with Not_found ->
    try RV32A.T.pretty i with Not_found ->
    try RV32F.T.pretty i with Not_found ->
    try RV32D.T.pretty i with Not_found ->
        RVSYS.T.pretty i 

  let fields = 
    List.concat 
      [
        (RV64I.T.fields :> ((t * Types.Fields.t list) list));
        (RV64M.T.fields :> ((t * Types.Fields.t list) list));
        (RV64A.T.fields :> ((t * Types.Fields.t list) list));
        (RV64F.T.fields :> ((t * Types.Fields.t list) list));
        (RV64D.T.fields :> ((t * Types.Fields.t list) list));
        (RV32M.T.fields :> ((t * Types.Fields.t list) list));
        (RV32A.T.fields :> ((t * Types.Fields.t list) list));
        (RV32F.T.fields :> ((t * Types.Fields.t list) list));
        (RV32D.T.fields :> ((t * Types.Fields.t list) list));
        (RVSYS.T.fields :> ((t * Types.Fields.t list) list));
      ]

end

module Asm = struct
  include RV64I.Asm
  include RV64M.Asm
  include RV64A.Asm
  include RV64F.Asm
  include RV64D.Asm
  include RV32M.Asm
  include RV32A.Asm
  include RV32F.Asm
  include RV32D.Asm
  include RVSYS.Asm
end

module Test = struct
  let suite f n = 
    let suites = [
      (RV64M.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV64M.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV64A.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV64F.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV64D.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV32M.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV32A.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV32F.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RV32D.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
      (RVSYS.Test.suite :> ((T.t -> Types.I.t -> bool) -> int -> QCheck.suite));
    ] in
    List.concat (List.map (fun suite -> suite f n) suites)
end


