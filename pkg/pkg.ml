#!/usr/bin/env ocaml
#use "topfind"
#require "topkg,astring,rresult"
open Topkg

(* given a module name (and file path), try to figure out if
   the corresponding .mli file is capitalized or not. *)
let mli_of_module ?(check_ml=false) dir m = 
  let check f ext = 
    let m' = f m in
    let name = Fpath.(dir // m' ^ ext) in
    match OS.File.exists name with
    | Ok true -> Ok(Some(m'))
    | Ok false -> Ok(None)
    | Error e -> Error e
  in
  let (>>:) a (c, f, ext) = a >>= function
    | Some(m) as x -> Ok x
    | None -> if c then check f ext else Ok None
  in
  (Ok(None) >>: (true, Astring.String.Ascii.uncapitalize, ".mli")
            >>: (true, Astring.String.Ascii.capitalize, ".mli")
            >>: (check_ml, Astring.String.Ascii.uncapitalize, ".ml")
            >>: (check_ml, Astring.String.Ascii.capitalize, ".ml")
            >>= function None -> Error(`Msg("mli_of_module: " ^ m))
                       | Some(m) -> Ok m)

let mlpack ?cond name =  
  let dir = Fpath.dirname name in
  let base = Fpath.basename name in
  let parse contents =
    let lines = String.cuts ~sep:'\n' contents in
    let add_mod acc l =
      let m = String.trim @@ match String.cut ~sep:'#' l with
      | None -> l
      | Some (m, _ (* comment *)) -> m
      in
      if m = "" then acc else m :: acc
    in
    Ok (List.fold_left add_mod [] lines)
  in
  let modls = (* modules within the pack *)
    let name = Fpath.(dir // base ^ ".mlpack") in
    OS.File.read name >>= parse
  in
  let intf modls = (* install interfaces for modules in the library - .cmti/.mli *)
    Ok (List.map 
      (fun m ->
        let cond, m = 
          match mli_of_module dir m with
          | Error _ -> Some(false), m
          | Ok m -> cond, m
        in
        let name = Fpath.(dir // m) in
        Pkg.lib ?cond ~exts:Exts.(exts [".cmti"; ".mli"]) name
      ) modls)
  in
  let name = Fpath.(dir // base) in
  let pkg l = 
    let lib = 
      Pkg.lib ?cond 
        ~exts:Exts.(exts [".a"; ".cmi"; ".cma"; ".cmx"; ".cmxa"; ".cmxs"; ".cmti"]) 
        name 
    in
    Ok (lib :: l)
  in
  (modls >>= intf >>= pkg) |> Log.on_error_msg ~use:(fun () -> [])

let () = 
  Pkg.describe "riscv" @@ fun c ->
  Ok (
    mlpack "src/Riscv" 
  )

