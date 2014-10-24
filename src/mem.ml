exception RISCV_store_size_too_big
exception RISCV_load_size_too_big

exception RISCV_store_address_misaligned of int * int
exception RISCV_load_address_misaligned of int * int

module Make(T : Cpu.T) = struct
  
  open T

  let insert_bits m a d pos size = 
    let mask' = D.mask size in
    D.A.set m a D.( ((D.A.get m a) &: (~: (mask' <<: pos))) |: ((d &: mask') <<: pos) )

  let mask n = (1 lsl n) - 1

  let store lbytes m a d =
    (* check size *)
    if lbytes > D.lbytes then raise RISCV_store_size_too_big
    else 
      let am = a land (mask D.lbytes) in
      let ad = a lsr D.lbytes in
      (* alignment *)
      if (am land (mask lbytes)) <> 0 then  raise (RISCV_store_address_misaligned(lbytes, a))
      (* full word *)
      else if lbytes = D.lbytes then D.A.set m ad d
      (* partial word *)
      else insert_bits m ad d (am lsl 3) (1 lsl (lbytes+3))

  let load lbytes m a = 
    (* check size *)
    if lbytes > D.lbytes then raise RISCV_load_size_too_big
    else 
      let am = a land (mask D.lbytes) in
      let ad = a lsr D.lbytes in
      (* alignment *)
      if (am land (mask lbytes)) <> 0 then  raise (RISCV_load_address_misaligned(lbytes, a))
      (* full word *)
      else if lbytes = D.lbytes then D.A.get m ad 
      (* partial word *)
      else 
        let mask' = D.mask (1 lsl (lbytes+3)) in
        D.( (D.A.get m ad >>: (am lsl 3)) &: mask' )

end

