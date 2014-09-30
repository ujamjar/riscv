module T = struct

  type t = [
    | RV32G.T.t
    | RV64I.T.t
    | RV64M.T.t
    | RV64A.T.t
    | RV64F.T.t
    | RV64D.T.t
  ]

  let mask_match = 
    List.concat
      [
        (RV32G.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64I.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64M.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64A.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64F.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
        (RV64D.T.mask_match :> ((t * (Int32.t * Int32.t))) list);
      ]

  let to_t i = 
    let rec f = function
      | [] -> raise Not_found
      | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
    in
    f mask_match

  let pretty i = 
    try RV32G.T.pretty i 
    with Not_found -> 
    try RV64I.T.pretty i 
    with Not_found -> 
    try RV64M.T.pretty i 
    with Not_found -> 
    try RV64A.T.pretty i 
    with Not_found -> 
    try RV64F.T.pretty i 
    with Not_found -> 
    try RV64D.T.pretty i 
    with Not_found -> raise Not_found

end

module Asm = struct
  include RV32G.Asm
  include RV64I.Asm
  include RV64M.Asm
  include RV64A.Asm
  include RV64F.Asm
  include RV64D.Asm
end

