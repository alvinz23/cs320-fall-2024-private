(* test_assign00.ml *)
open OUnit2

let tests = "Testing Assign00" >:::
  [ Test01.test_examples   (* Ensure Test01.test_examples is included *)
  ; Test02.test_examples   (* Ensure Test02.test_examples is included if needed *)
  ]

let _ = run_test_tt_main tests
