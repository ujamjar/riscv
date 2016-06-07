open Printf

let modules = [
  "rv32i"; "rv64i"; 
  "rv32a"; "rv64a"; 
  "rv32m"; "rv64m"; 
  "rv32f"; "rv64f"; 
  "rv32d"; "rv64d";
  "rvc";
  "rvsuper"; "rvmachine";
]

let mask p =
  let open Opcodes in
  List.fold_left (fun (mask,matcher) p ->
    match p with
    | Bit(n,Int x) ->
      Int32.logor mask Int32.(shift_left 1l n),
      Int32.logor matcher Int32.(shift_left (of_int x) n)

    | Range((h,l),Int x) ->
      Int32.logor mask Int32.(shift_left (sub (shift_left 1l (h-l+1)) 1l) l),
      Int32.logor matcher Int32.(shift_left (of_int x) l)

    | _ -> mask,matcher) (0l,0l) p

let write_instrs_type f instrs = 
  (* type *)
  fprintf f "type t = [\n";
  List.iter (fun (n,_) -> fprintf f "| `%s\n" (Opcodes.map_name n)) instrs;
  fprintf f "] deriving(Enum,Bounded,Show)\n\n"

let write_mask f instrs = 
  fprintf f "let mask_match = [\n";
  List.iter (fun (n,p) ->
    let mask,matcher = mask p in
    fprintf f "  `%-8s, (0x%.8lxl,0x%.8lxl);\n" (Opcodes.map_name n) mask matcher
  ) instrs;
  fprintf f "]\n"

let write_get_opcode f = 
  fprintf f "
let to_t i = 
  let rec f = function
    | [] -> raise Not_found
    | (op,(m,m'))::t -> if Int32.logand m i = m' then op else f t
  in
  f mask_match

"

let write_pretty f instrs = 
  let open Opcodes in
  fprintf f "let pretty i =\n";
  fprintf f "  let x h l = 
    Printf.sprintf \"0x%%lx\" 
      Int32.(logand (shift_right i l) (sub (shift_left 1l (h-l+1)) 1l)) 
  in
";
  fprintf f "  match to_t i with\n";
  List.iter (fun (n,p) ->
    fprintf f "  | `%-8s ->\n" (Opcodes.map_name n);
    fprintf f "    (\"%s\"" n;
    List.iter (function 
      | Field((_,n,(h,l)),Ignore) 
      | Field((_,n,(h,l)),Nothing) -> 
          fprintf f " ^ \" %s=\" ^ (x %i %i)" n h l
      | _ -> ()) p;
    fprintf f ")\n"
  ) instrs

let write_fields f instrs = 
  let value = function
    | Opcodes.Ignore -> "Ignore"
    | Opcodes.Nothing -> "Nothing"
    | Opcodes.Int i -> sprintf "Int(%i)" i
  in
  fprintf f "let fields =\n";
  fprintf f "  let open Types.Fields in\n";
  fprintf f "  [\n";
  List.iter (fun (n,p) ->
    fprintf f "    (`%s, [ " (Opcodes.map_name n);
    List.iter (function
      | Opcodes.Field((_,n,(h,l)), v) ->
        fprintf f "Field((`%s,\"%s\",(%i,%i)), %s); " n n h l (value v)
      | Opcodes.Bit(n,v) ->
        fprintf f "Bit(%i,%s); " n (value v)
      | Opcodes.Range((h,l),v) ->
        fprintf f "Range((%i,%i),%s); " h l (value v)
    ) p;
    fprintf f "]);\n"
  ) instrs;
  fprintf f "  ]\n\n"

let write_module m = 

  printf "generating module: %s\n" m;

  let ops = open_in ("tools/" ^ m ^ ".ops") in
  let mli = open_out ("src/" ^ String.uppercase m ^ ".mli") in 
  let ml = open_out ("src/" ^ String.uppercase m ^ ".ml") in 

  let instrs = Opcodes.load ops in

  fprintf mli "module T : sig\n\n";
  write_instrs_type mli instrs;
  fprintf mli "val name : string\n\n";
  fprintf mli "val mask_match : (t * (Types.I.t * Types.I.t)) list\n\n";
  fprintf mli "val to_t : Types.I.t -> t\n\n";
  fprintf mli "val pretty : Types.I.t -> string\n\n";
  fprintf mli "val fields : (t * Types.Fields.t list) list\n\n";
  fprintf mli "end\n\n";

  fprintf mli "module Asm_raw : sig\n\n";
  Opcodes.write_asm_raw_mli mli instrs;
  fprintf mli "\nend\n\n";

  fprintf mli "module Asm : sig\n\n";
  Opcodes.write_asm_mli mli instrs;
  fprintf mli "\nend\n\n";

  fprintf mli "module Test : sig\n\n";
  fprintf mli "val suite : (T.t -> Types.I.t -> bool) -> int -> QCheck.suite\n";
  fprintf mli "\nend\n\n";

  fprintf ml "module T = struct\n\n";
  write_instrs_type ml instrs;
  fprintf ml "let name = \"%s\"\n\n" m;
  write_mask ml instrs;
  write_get_opcode ml;
  write_pretty ml instrs;
  write_fields ml instrs;
  fprintf ml "end\n\n";

  fprintf ml "module Asm_raw = struct\n\n";
  Opcodes.write_asm_raw_ml ml instrs;
  fprintf ml "end\n\n";

  fprintf ml "module Asm = struct\n\n";
  Opcodes.write_asm_ml ml instrs;
  fprintf ml "end\n\n";

  fprintf ml "module Test = struct\n\n";
  Opcodes.write_qcheck_suite ml instrs;
  fprintf ml "end\n\n";

  close_in ops;
  close_out ml;
  close_out mli

let () = List.iter write_module modules


