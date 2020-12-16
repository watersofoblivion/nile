open Format
open OUnit2
open Nile

let assert_type_equal ~ctxt expected actual =
  let cmp _ _ = false in
  let msg = "Types are not equal" in
  let printer ty =
    ty
      |> Type.pp
      |> fprintf str_formatter "%t"
      |> flush_str_formatter
  in
  let rec assert_type_equal expected actual = match expected, actual with
    | Type.Int loc, Type.Int loc'
    | Type.Bool loc, Type.Bool loc' -> LocTest.assert_loc_equal ~ctxt loc loc'
    | Type.Fun (loc, a, b), Type.Fun (loc', a', b') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      assert_type_equal a a';
      assert_type_equal b b'
    | expected, actual ->
      assert_equal ~ctxt ~cmp ~printer ~msg expected actual
  in
  assert_type_equal expected actual

let suite =
  let test_constructor =
    let fail_type expected actual =
      fprintf str_formatter "Expected \"%s\", found \"%t\"" expected (Type.pp actual)
        |> flush_str_formatter
        |> assert_failure
    in

    let test_int ctxt =
      match Type.int LocTest.dummy with
        | Type.Int loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | t -> fail_type "Int" t
    in
    let test_bool ctxt =
      match Type.bool LocTest.dummy with
        | Type.Bool loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | t -> fail_type "Bool" t
    in
    let test_fun ctxt =
      let a = Type.int LocTest.dummy in
      let b = Type.bool LocTest.dummy in
      match Type.func LocTest.dummy a b with
        | Type.Fun (loc, a', b') ->
          LocTest.assert_loc_equal ~ctxt LocTest.dummy loc;
          assert_type_equal ~ctxt a a';
          assert_type_equal ~ctxt b b'
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
    let test_int ctxt = assert_pp ~ctxt (Type.int LocTest.dummy) "Int" in
    let test_bool ctxt = assert_pp ~ctxt (Type.bool LocTest.dummy) "Bool" in
    let test_fun =
      let f = Type.func LocTest.dummy (Type.int LocTest.dummy) (Type.bool LocTest.dummy) in

      let test_simple ctxt = assert_pp ~ctxt f "Int -> Bool" in
      let test_higher_order ctxt =
        let f = Type.func LocTest.dummy f f in
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
    let i = Type.int LocTest.dummy in
    let b = Type.bool LocTest.dummy in
    let f = Type.func LocTest.dummy i b in
    let hof = Type.func LocTest.dummy f f in

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
