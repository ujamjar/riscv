module T = struct

type t = [
| `sfence_vm
] deriving(Enum,Bounded,Show)

let name = "rvsuper"

let mask_match = [
  `sfence_vm, (0xfff07fffl,0x10100073l);
]

let to_t i = 
  let rec f = function
    | [] -> raise Not_found
    | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
  in
  f mask_match

let pretty i =
  let x h l = 
    Printf.sprintf "0x%lx" 
      Int32.(logand (shift_right i l) (sub (shift_left 1l (h-l+1)) 1l)) 
  in
  match to_t i with
  | `sfence_vm ->
    ("sfence.vm" ^ " rs1=" ^ (x 19 15))
let fields =
  let open Types.Fields in
  [
    (`sfence_vm, [ Range((11,7),Int(0)); Field((`rs1,"rs1",(19,15)), Nothing); Range((31,20),Int(257)); Range((14,12),Int(0)); Range((6,2),Int(28)); Range((1,0),Int(3)); ]);
  ]

end

module Asm_raw = struct

let sfence_vm ~rs1 = Types.I.(
  (sll ((of_int rs1) &: 0x1fl) 15) |:
  0x10100073l)

end

module Asm = struct

let sfence_vm = Asm_raw.sfence_vm
end

module Test = struct

let suite f n = [
  QCheck.( mk_test ~name:"sfence.vm" ~n 
    ~pp:PP.(QCRV.PP.tuple1 int) ~limit:2
    Arbitrary.(QCRV.tuple1 (int 32)) 
    (fun (rs1) -> f `sfence_vm (Asm_raw.sfence_vm ~rs1)));
]

end

