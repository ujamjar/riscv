module T = struct

  type t = [
    | RV32I.T.t
    | RV32M.T.t
    | RV32A.T.t
    | RV32F.T.t
    | RV32D.T.t
  ]

  let mask_match = 
    List.concat
      [
        (RV32I.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32M.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32A.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32F.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV32D.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
      ]

  let to_t i = 
    let rec f = function
      | [] -> raise Not_found
      | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
    in
    f mask_match

  let pretty i = 
    try RV32I.T.pretty i 
    with Not_found -> 
    try RV32M.T.pretty i 
    with Not_found -> 
    try RV32A.T.pretty i 
    with Not_found -> 
    try RV32F.T.pretty i 
    with Not_found -> 
    try RV32D.T.pretty i 
    with Not_found -> raise Not_found

end

module Asm = struct
  include RV32I.Asm
  include RV32M.Asm
  include RV32A.Asm
  include RV32F.Asm
  include RV32D.Asm
end

