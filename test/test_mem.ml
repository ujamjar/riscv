(**********************************************************************)
(* test the memory primitives *)

let size_bytes = 1024
let size_halfs = size_bytes/2
let size_words = size_halfs/2
let size_doubles = size_words/2

(**********************************************************************)
(* 64 bits *)

module T64 = Cpu.Make(Types.D64)
module M64 = Mem.Make(T64)
open T64

let m64 = D.A.create (size_bytes/8) 0L
let clear64 () = 
  for i=0 to (size_bytes/8)-1 do
    D.A.set m64 i 0L
  done

(* byte tests *)
let () = begin
  (* set one byte to 0xFF across whole memory - read and check *)
  for i=0 to size_bytes-1 do
    D.A.set m64 (i/8) D.(0xFFL <<: ((i mod 8)*8));
    assert (M64.load 0 m64 i = 0xFFL);
  done;
  (* as above, but all other bytes are 0xAA - checks masking *)
  for i=0 to size_bytes-1 do
    D.A.set m64 (i/8) D.((0xFFL <<: ((i mod 8)*8)) |: 0xAAAA_AAAA_AAAA_AAAAL);
    assert (M64.load 0 m64 i = 0xFFL);
  done;
  (* set each byte to an incrementing counter and check *)
  for i=0 to size_bytes-1 do
    let v = D.of_int (i mod 256) in
    M64.store 0 m64 i v;
    assert (M64.load 0 m64 i = v);
  done;
end

(* half tests *)
let () = begin
  (* set one half to 0xFFFF across whole memory - read and check *)
  for i=0 to size_halfs-1 do
    D.A.set m64 (i/4) D.(0xFFFFL <<: ((i mod 4)*16));
    assert (M64.load 1 m64 (i*2) = 0xFFFFL);
  done;
  (* as above, but all other halfs are 0xAA - checks masking *)
  for i=0 to size_halfs-1 do
    D.A.set m64 (i/4) D.((0xFFFFL <<: ((i mod 4)*16)) |: 0xAAAA_AAAA_AAAA_AAAAL);
    assert (M64.load 1 m64 (i*2) = 0xFFFFL);
  done;
  (* set each half to an incrementing counter and check *)
  for i=0 to size_halfs-1 do
    let v = D.of_int (i mod (1 lsl 16)) in
    M64.store 1 m64 (i*2) v;
    assert (M64.load 1 m64 (i*2) = v);
  done;
end

(* word tests *)
let () = begin
  (* set one word to 0xFFFF_FFFF across whole memory - read and check *)
  for i=0 to size_words-1 do
    D.A.set m64 (i/2) D.(0xFFFF_FFFFL <<: ((i mod 2)*32));
    assert (M64.load 2 m64 (i*4) = 0xFFFF_FFFFL);
  done;
  (* as above, but all other words are 0xAA - checks masking *)
  for i=0 to size_words-1 do
    D.A.set m64 (i/2) D.((0xFFFF_FFFFL <<: ((i mod 2)*32)) |: 0xAAAA_AAAA_AAAA_AAAAL);
    assert (M64.load 2 m64 (i*4) = 0xFFFF_FFFFL);
  done;
  (* set each word to an incrementing counter and check *)
  for i=0 to size_words-1 do
    let v = D.of_int i in
    M64.store 2 m64 (i*4) v;
    assert (M64.load 2 m64 (i*4) = v);
  done;
end

(* double tests *)
let () = begin
  (* set one double to 0xFF..FF across whole memory - read and check *)
  for i=0 to size_doubles-1 do
    D.A.set m64 i D.(0xFFFF_FFFF_FFFF_FFFFL);
    assert (M64.load 3 m64 (i*8) = 0xFFFF_FFFF_FFFF_FFFFL);
  done;
  (* set each double to an incrementing counter and check *)
  for i=0 to size_doubles-1 do
    let v = D.of_int i in
    M64.store 3 m64 (i*8) v;
    assert (M64.load 3 m64 (i*8) = v);
  done;
end

(**********************************************************************)
(* 32 bits *)

module T32 = Cpu.Make(Types.D32)
module M32 = Mem.Make(T32)
open T32

let m32 = D.A.create (size_bytes/4) 0l
let clear32 () = 
  for i=0 to (size_bytes/4)-1 do
    D.A.set m32 i 0l
  done

(* byte tests *)
let () = begin
  (* set one byte to 0xFF across whole memory - read and check *)
  for i=0 to size_bytes-1 do
    D.A.set m32 (i/4) D.(0xFFl <<: ((i mod 4)*8));
    assert (M32.load 0 m32 i = 0xFFl);
  done;
  (* as above, but all other bytes are 0xAA - checks masking *)
  for i=0 to size_bytes-1 do
    D.A.set m32 (i/4) D.((0xFFl <<: ((i mod 4)*8)) |: 0xAAAA_AAAAl);
    assert (M32.load 0 m32 i = 0xFFl);
  done;
  (* set each byte to an incrementing counter and check *)
  for i=0 to size_bytes-1 do
    let v = D.of_int (i mod 256) in
    M32.store 0 m32 i v;
    assert (M32.load 0 m32 i = v);
  done;
end

(* half tests *)
let () = begin
  (* set one half to 0xFFFF across whole memory - read and check *)
  for i=0 to size_halfs-1 do
    D.A.set m32 (i/2) D.(0xFFFFl <<: ((i mod 2)*16));
    assert (M32.load 1 m32 (i*2) = 0xFFFFl);
  done;
  (* as above, but all other halfs are 0xAA - checks masking *)
  for i=0 to size_halfs-1 do
    D.A.set m32 (i/2) D.((0xFFFFl <<: ((i mod 2)*16)) |: 0xAAAA_AAAAl);
    assert (M32.load 1 m32 (i*2) = 0xFFFFl);
  done;
  (* set each half to an incrementing counter and check *)
  for i=0 to size_halfs-1 do
    let v = D.of_int (i mod (1 lsl 16)) in
    M32.store 1 m32 (i*2) v;
    assert (M32.load 1 m32 (i*2) = v);
  done;
end

(* word tests *)
let () = begin
  (* set one word to 0xFFFF_FFFF across whole memory - read and check *)
  for i=0 to size_words-1 do
    D.A.set m32 i D.(0xFFFF_FFFFl);
    assert (M32.load 2 m32 (i*4) = 0xFFFF_FFFFl);
  done;
  (* set each word to an incrementing counter and check *)
  for i=0 to size_words-1 do
    let v = D.of_int i in
    M32.store 2 m32 (i*4) v;
    assert (M32.load 2 m32 (i*4) = v);
  done;
end

