open Instr.Util

module A = RV64G
let decode = Instr.Util.instruction_decoder A.T.mask_match
let decode x i = try x = decode i with Not_found -> false
let _ = QCheck.run_tests (A.Test.suite decode 10000)

module B = RV32G
let decode = Instr.Util.instruction_decoder B.T.mask_match
let decode x i = try x = decode i with Not_found -> false
let _ = QCheck.run_tests (B.Test.suite decode 10000)

module C = RV32I
let decode = Instr.Util.instruction_decoder C.T.mask_match
let decode x i = try x = decode i with Not_found -> false
let _ = QCheck.run_tests (C.Test.suite decode 10000)

