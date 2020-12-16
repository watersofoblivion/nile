open OUnit2
open Ir

let assert_conf ~ctxt tailcall inline ccp max_passes actual =
  assert_equal ~ctxt ~printer:string_of_bool tailcall actual.Opt.tailcall ~msg:"Tail-call optimization configuration is not correct";
  assert_equal ~ctxt ~printer:string_of_bool inline actual.inline ~msg:"Inlining configuration is not correct";
  assert_equal ~ctxt ~printer:string_of_bool ccp actual.ccp ~msg:"Conditional constant propagation configuration is not correct";
  assert_equal ~ctxt ~printer:string_of_int max_passes actual.max_passes ~msg:"Maximum passes are not equal"

let suite =
  let test_conf =
    let test_tailcall ctxt =
      Opt.conf true false false 0
        |> assert_conf ~ctxt true false false 0
    in
    let test_inline ctxt =
      Opt.conf false true false 0
        |> assert_conf ~ctxt false true false 0
    in
    let test_ccp ctxt =
      Opt.conf false false true 0
        |> assert_conf ~ctxt false false true 0
    in
    let test_max_passes ctxt =
      Opt.conf false false false 10
        |> assert_conf ~ctxt false false false 10
    in
    "Configuration" >::: [
      "Tail-call Optimization"           >:: test_tailcall;
      "Inliner"                          >:: test_inline;
      "Conditional Constant Propagation" >:: test_ccp;
      "Maximum Passes"                   >:: test_max_passes;
    ]
  in
  "Optimizer" >::: [
    test_conf;
  ]
