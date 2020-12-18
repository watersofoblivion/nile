open Format
open OUnit2
open Ir

let never _ _ = false
let printer pp x =
  x
    |> pp
    |> fprintf str_formatter "%t"
    |> flush_str_formatter

let rec assert_type_equal ~ctxt expected actual = match expected, actual with
  | Type.Bool, Type.Bool
  | Type.Int, Type.Int -> ()
  | Type.Fun (a, b), Type.Fun (a', b') ->
    assert_type_equal ~ctxt a a';
    assert_type_equal ~ctxt b b'
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Type.pp) ~msg:"Types are not equal" expected actual

let assert_pp pp ~ctxt lines anf =
  let printer str =
    let map str = sprintf "%S" str in
    str
      |> String.split_on_char '\n'
      |> List.map map
      |> String.concat "\n"
  in
  let expected = String.concat "\n" lines in
  fprintf str_formatter "%t" (pp anf)
    |> flush_str_formatter
    |> assert_equal ~ctxt ~printer expected

let suite =
  let test_constructor =
    let printer = printer Type.pp in

    let test_bool ctxt =
      let ty = Type.bool in
      match ty with
        | Type.Bool -> ()
        | ty' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Types are not equal" ty ty'
    in
    let test_int ctxt =
      let ty = Type.int in
      match ty with
        | Type.Int -> ()
        | ty' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Types are not equal" ty ty'
    in
    let test_func ctxt =
      let a = Type.int in
      let b = Type.bool in
      let ty = Type.func a b in
      match ty with
        | Type.Fun (a', b') ->
          assert_type_equal ~ctxt a a';
          assert_type_equal ~ctxt b b'
        | ty' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Types are not equal" ty ty'
    in
    "Constructor" >::: [
      "Boolean"  >:: test_bool;
      "Integer"  >:: test_int;
      "Function" >:: test_func;
    ]
  in
  let test_pp =
    let assert_pp = assert_pp Type.pp in

    let test_bool ctxt =
      Type.bool
        |> assert_pp ~ctxt ["Bool"]
    in
    let test_int ctxt =
      Type.int
        |> assert_pp ~ctxt ["Int"]
    in
    let test_func ctxt =
      Type.func Type.int Type.int
        |> assert_pp ~ctxt ["Int -> Int"];
      Type.func Type.int (Type.func Type.int Type.int)
        |> assert_pp ~ctxt ["Int -> Int -> Int"];
      Type.func (Type.func Type.int Type.int) Type.int
        |> assert_pp ~ctxt ["(Int -> Int) -> Int"]
    in
    "Pretty-Printer" >::: [
      "Boolean"  >:: test_bool;
      "Integer"  >:: test_int;
      "Function" >:: test_func;
    ]
  in
  "Types" >::: [
    test_constructor;
    test_pp;
  ]
