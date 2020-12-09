open Format
open OUnit2
open Nile

let suite =
  let test_constructor =
    let fail_type expected actual =
      fprintf str_formatter "Expected \"%s\", found \"%t\"" expected (Type.pp actual)
        |> flush_str_formatter
        |> assert_failure
    in

    let test_int _ =
      match Type.int with
        | Type.Int -> ()
        | t -> fail_type "Int" t
    in
    let test_bool _ =
      match Type.bool with
        | Type.Bool -> ()
        | t -> fail_type "Bool" t
    in
    let test_fun _ =
      let a = Type.int in
      let b = Type.bool in
      match Type.func a b with
        | Type.Fun (Type.Int, Type.Bool) -> ()
        | t -> fail_type "Int -> Bool" t
    in
    "Constructors" >::: [
      "Integers"  >:: test_int;
      "Booleans"  >:: test_bool;
      "Functions" >:: test_fun;
    ]
  in
  let test_pp =
    let assert_pp ~ctxt ty expected =
      Type.pp ty str_formatter
        |> flush_str_formatter
        |> assert_equal ~ctxt expected
    in
    let test_int ctxt = assert_pp ~ctxt Type.int "Int" in
    let test_bool ctxt = assert_pp ~ctxt Type.bool "Bool" in
    let test_fun =
      let f = Type.func Type.int Type.bool in

      let test_simple ctxt = assert_pp ~ctxt f "Int -> Bool" in
      let test_higher_order ctxt =
        let f = Type.func f f in
        assert_pp ~ctxt f "(Int -> Bool) -> Int -> Bool"
      in
      "Functions" >::: [
        "Simple"       >:: test_simple;
        "Higher-Order" >:: test_higher_order
      ]
    in
    "Pretty Printer" >::: [
      "Integers" >:: test_int;
      "Booleans" >:: test_bool;
      test_fun;
    ]
  in
  let test_equal =
    let i = Type.int in
    let b = Type.bool in
    let f = Type.func i b in
    let hof = Type.func f f in

    let assert_ty_equal ~ctxt expected actual =
      assert_equal ~ctxt ~cmp:Type.equal expected actual
    in
    let assert_ty_not_equal ~ctxt expected actual =
      let cmp x y = not (Type.equal x y) in
      assert_equal ~ctxt ~cmp expected actual
    in

    let test_int ctxt =
      assert_ty_equal ~ctxt i i;
      assert_ty_not_equal ~ctxt i b;
      assert_ty_not_equal ~ctxt i f;
      assert_ty_not_equal ~ctxt i hof
    in
    let test_bool ctxt =
      assert_ty_not_equal ~ctxt b i;
      assert_ty_equal ~ctxt b b;
      assert_ty_not_equal ~ctxt b f;
      assert_ty_not_equal ~ctxt b hof
    in
    let test_fun =
      let test_simple ctxt =
        assert_ty_not_equal ~ctxt f i;
        assert_ty_not_equal ~ctxt f b;
        assert_ty_equal ~ctxt f f;
        assert_ty_not_equal ~ctxt f hof
      in
      let test_higher_order ctxt =
        assert_ty_not_equal ~ctxt hof i;
        assert_ty_not_equal ~ctxt hof b;
        assert_ty_not_equal ~ctxt hof f;
        assert_ty_equal ~ctxt hof hof
      in
      "Functions" >::: [
        "Simple"       >:: test_simple;
        "Higher-Order" >:: test_higher_order
      ]
    in
    "Equality" >::: [
      "Integers"  >:: test_int;
      "Booleans"  >:: test_bool;
      test_fun;
    ]
  in
  "Types" >::: [
    test_constructor;
    test_pp;
    test_equal;
  ]
