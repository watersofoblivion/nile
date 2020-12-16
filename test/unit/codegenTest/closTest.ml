open OUnit2
open Codegen

let assert_mode ~ctxt expected actual =
  let cmp _ _ = false in
  let msg = "Closure conversion modes are not equal" in
  let printer = function
    | Clos.Flat -> "flat"
    | Clos.Linked -> "linked"
    | Clos.SSC -> "ssc"
  in
  match expected, actual with
    | Clos.Flat, Clos.Flat -> ()
    | Clos.Linked, Clos.Linked -> ()
    | Clos.SSC, Clos.SSC -> ()
    | expected, actual -> assert_equal ~ctxt ~cmp ~printer ~msg expected actual

let assert_conf ~ctxt mode actual =
  assert_mode ~ctxt mode actual.Clos.mode

let suite =
  let test_conf =
    let test_mode ctxt =
      let conf = Clos.conf Clos.Flat in
      assert_mode ~ctxt Clos.Flat conf.mode
    in
    "Configuration" >::: [
      "Mode" >:: test_mode;
    ]
  in
  "Closure Conversion" >::: [
    test_conf;
  ]
