(* parse the risc-v opcodes files *)
open Re

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

let get x = List.find (fun (y,_,_) -> x=y) fields
let name x = List.find (fun (_,y,_) -> x=y) fields

let re_split at = compile (seq [ group (shortest (rep1 any)); at; (group (rep1 any)) ])
let re_spaces = compile (seq [rep1 (char ' '); eos])

let split at = 
  let re = compile (seq [ rep at; group (rep1 (compl [at])); group (rep any) ]) in
  let rec f str = 
    match get_all (exec re str) with
    | [| _; m; r |] -> m :: f r
    | _ -> []
    | exception Not_found -> []
  in
  f

let split2 at = 
  let re = compile (seq [ group (rep any); at; group (rep any); eos ]) in
  let rec f str = 
    match get_all (exec re str) with
    | [| _; a; b |] -> Some(a,b)
    | _ -> None
    | exception Not_found -> None
  in
  f

let comment = 
  let re_comment = compile (seq [ group (shortest (rep any)); char '#'; rep any ]) in
  fun str ->
    match get_all (exec re_comment str) with
    | [|a;b|] -> b
    | _ -> raise Not_found
    | exception Not_found -> str

let value = 
  let split = split2 (char '=') in
  let f s = 
    match split s with
    | Some(l, r) -> Some (l, r)
    | None -> None
  in
  f

let re_num = compile (seq [bos; (rep1 digit) ])

let bit = 
  let f s = 
    match get_all (exec re_num s) with
    | [| n |] -> Some( int_of_string n )
    | _ -> None
    | exception Not_found -> None
  in
  f

let range = 
  let split = split2 (str "..") in
  let f s = 
    match split s with
    | Some(h, l) -> Some(int_of_string h, int_of_string l)
    | None  -> None
  in
  f

let value_of_string x = 
  if x = "ignore" then Ignore
  else Int(int_of_string x)

let spec s = 
  match value s with
  | Some(l,r) -> begin
    match range l with
    | Some(h,l) -> Range((h,l), (value_of_string r))
    | None -> begin
      match bit s with 
      | Some b -> Bit(b, (value_of_string r))
      | None -> Field(name l, value_of_string r)
    end
  end
  | None -> Field(name s, Nothing)

let tokens s = 
  split (char ' ') (comment s)

let parse s = 
  let tokens = tokens s in
  match tokens with
  | [] -> None
  | h::t -> Some(h,List.map spec t)

let load f = 
  let rec p line instrs = 
    match input_line f with
    | _ as s -> begin
      match parse s with
      | Some(x) -> x :: p (line+1) instrs
      | None -> p (line+1) instrs
      | exception e -> begin
        Printf.printf "exn[%i]: %s\n" line (Printexc.to_string e);
        exit 1
      end
    end
    | exception _ -> List.rev instrs
  in
  let instrs = p 1 [] in
  instrs

let map_name n = 
  match n with
  | "or" -> "or_"
  | "and" -> "and_"
  | "xor" -> "xor_"
  | _ as x -> String.map (function '.' -> '_' | _ as x -> x) x

let write_asm_ml f instrs = 
  let open Printf in
  List.iter (fun (n,p) ->
    (* function header *)
    fprintf f "let %s " (map_name n);
    List.iter (function
      | Field((_,n,(_,_)),Ignore) 
      | Field((_,n,(_,_)),Nothing) -> fprintf f "~%s " n
      | _ -> ()) p;
    fprintf f "=\n";
    (* function body *)
    let v = List.fold_left (fun acc i ->
      let m i n = Int32.(logor acc (shift_left (of_int i) n)) in
      match i with
      | Field((_,n,(h,l)),Ignore) 
      | Field((_,n,(h,l)),Nothing) -> 
          fprintf f "  I.(((to_int %s) &: 0x%xl) <<: %i) |:\n" n ((1 lsl (h-l+1))-1) l;
          acc
      | Field((_,_,(_,l)),Int d) -> m d l

      | Bit(n,Ignore) 
      | Bit(n,Nothing) -> acc
      | Bit(n,Int d) -> m d n

      | Range((_,l), Ignore) 
      | Range((_,l), Nothing) -> acc
      | Range((_,l), Int d) -> m d l
      ) 0l p
    in
    fprintf f "  0x%lxl\n\n" v
  ) instrs

let write_asm_mli f instrs = 
  let open Printf in
  List.iter (fun (n,p) ->
    printf "val %s : " (map_name n);
    List.iter (function
      | Field((_,n,(_,_)),Ignore) 
      | Field((_,n,(_,_)),Nothing) -> fprintf f "%s:int -> " n
      | _ -> ()) p;
    printf "I.t\n"
  ) instrs

let instrs = load stdin
let () = write_asm_ml stdout instrs
let () = write_asm_mli stdout instrs


