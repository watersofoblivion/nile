open Format
open OUnit2
open Common

let rec assert_type_equal ~ctxt expected actual = match expected, actual with
  | Type.Unit, Type.Unit
  | Type.Bool, Type.Bool
  | Type.Int, Type.Int -> ()
  | Type.Fun (a, b), Type.Fun (a', b') ->
    assert_type_equal ~ctxt a a';
    assert_type_equal ~ctxt b b'
  | Type.Tuple (len, tys), Type.Tuple (len', tys') ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Tuple sizes are not equal" len len';
    List.iter2 (assert_type_equal ~ctxt) tys tys'
  | expected, actual ->
    assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Type.pp) ~msg:"Types are not equal" expected actual

let suite =
  let test_constructor =
    let fail_type expected actual =
      fprintf str_formatter "Expected \"%s\", found \"%t\"" expected (Type.pp actual)
        |> flush_str_formatter
        |> assert_failure
    in

    let test_unit _ =
      match Type.unit with
        | Type.Unit -> ()
        | t -> fail_type "Unit" t
    in
    let test_bool _ =
      match Type.bool with
        | Type.Bool -> ()
        | t -> fail_type "Bool" t
    in
    let test_int _ =
      match Type.int with
        | Type.Int -> ()
        | t -> fail_type "Int" t
    in
    let test_fun ctxt =
      let a = Type.int in
      let b = Type.bool in
      match Type.func a b with
        | Type.Fun (a', b') ->
          assert_type_equal ~ctxt a a';
          assert_type_equal ~ctxt b b'
        | t -> fail_type "Int -> Bool" t
    in
    let test_tuple ctxt =
      let tys = [Type.unit; Type.bool; Type.int] in
      match Type.tuple tys with
        | Type.Tuple (len, tys') ->
          assert_equal ~ctxt ~printer:string_of_int ~msg:"Tuple sizes are not equal" (List.length tys) len;
          List.iter2 (assert_type_equal ~ctxt) tys tys'
        | t -> fail_type "(Unit, Bool, Int)" t
    in
    "Constructors" >::: [
      "Unit"      >:: test_unit;
      "Booleans"  >:: test_bool;
      "Integers"  >:: test_int;
      "Functions" >:: test_fun;
      "Tuples"    >:: test_tuple;
    ]
  in
  let test_pp =
    let assert_pp = Util.assert_pp Type.pp in

    let test_unit ctxt =
      Type.unit
        |> assert_pp ~ctxt ["Unit"]
    in
    let test_bool ctxt =
      Type.bool
        |> assert_pp ~ctxt ["Bool"]
    in
    let test_int ctxt =
      Type.int
        |> assert_pp ~ctxt ["Int"]
    in
    let test_fun =
      let f = Type.func Type.int Type.bool in

      let test_simple ctxt =
        f
          |> assert_pp ~ctxt ["Int -> Bool"]
      in
      let test_higher_order ctxt =
        Type.func f f
          |> assert_pp ~ctxt ["(Int -> Bool) -> Int -> Bool"]
      in
      "Functions" >::: [
        "Simple"       >:: test_simple;
        "Higher-Order" >:: test_higher_order
      ]
    in
    let test_tuple ctxt =
      Type.tuple [Type.unit; Type.bool; Type.int]
        |> assert_pp ~ctxt ["(Unit, Bool, Int)"]
    in
    "Pretty Printer" >::: [
      "Unit"      >:: test_unit;
      "Booleans"  >:: test_bool;
      "Integers"  >:: test_int;
      test_fun;
      "Tuples"    >:: test_tuple;
    ]
  in
  let test_equal =
    let u = Type.unit in
    let i = Type.int in
    let b = Type.bool in
    let f = Type.func i b in
    let hof = Type.func f f in
    let t0 = Type.tuple [u; i; b] in
    let t1 = Type.tuple [i; b; u] in
    let t2 = Type.tuple [i; i; i] in

    let assert_ty_equal ~ctxt expected actual =
      assert_equal ~ctxt ~cmp:Type.equal expected actual
    in
    let assert_ty_not_equal ~ctxt expected actual =
      let cmp x y = not (Type.equal x y) in
      assert_equal ~ctxt ~cmp expected actual
    in

    let test_unit ctxt =
      assert_ty_equal ~ctxt u u;
      assert_ty_not_equal ~ctxt u b;
      assert_ty_not_equal ~ctxt u i;
      assert_ty_not_equal ~ctxt u f;
      assert_ty_not_equal ~ctxt u hof;
      assert_ty_not_equal ~ctxt u t0;
      assert_ty_not_equal ~ctxt u t1;
      assert_ty_not_equal ~ctxt u t2
    in
    let test_bool ctxt =
      assert_ty_not_equal ~ctxt b u;
      assert_ty_equal ~ctxt b b;
      assert_ty_not_equal ~ctxt b i;
      assert_ty_not_equal ~ctxt b f;
      assert_ty_not_equal ~ctxt b hof;
      assert_ty_not_equal ~ctxt b t0;
      assert_ty_not_equal ~ctxt b t1;
      assert_ty_not_equal ~ctxt b t2
    in
    let test_int ctxt =
      assert_ty_not_equal ~ctxt i u;
      assert_ty_not_equal ~ctxt i b;
      assert_ty_equal ~ctxt i i;
      assert_ty_not_equal ~ctxt i f;
      assert_ty_not_equal ~ctxt i hof;
      assert_ty_not_equal ~ctxt i t0;
      assert_ty_not_equal ~ctxt i t1;
      assert_ty_not_equal ~ctxt i t2
    in
    let test_fun =
      let test_simple ctxt =
        assert_ty_not_equal ~ctxt f u;
        assert_ty_not_equal ~ctxt f b;
        assert_ty_not_equal ~ctxt f i;
        assert_ty_equal ~ctxt f f;
        assert_ty_not_equal ~ctxt f hof;
        assert_ty_not_equal ~ctxt f t0;
        assert_ty_not_equal ~ctxt f t1;
        assert_ty_not_equal ~ctxt f t2
      in
      let test_higher_order ctxt =
        assert_ty_not_equal ~ctxt hof u;
        assert_ty_not_equal ~ctxt hof b;
        assert_ty_not_equal ~ctxt hof i;
        assert_ty_not_equal ~ctxt hof f;
        assert_ty_equal ~ctxt hof hof;
        assert_ty_not_equal ~ctxt hof t0;
        assert_ty_not_equal ~ctxt hof t1;
        assert_ty_not_equal ~ctxt hof t2
      in
      "Functions" >::: [
        "Simple"       >:: test_simple;
        "Higher-Order" >:: test_higher_order
      ]
    in
    let test_tuple ctxt =
      assert_ty_not_equal ~ctxt t0 u;
      assert_ty_not_equal ~ctxt t0 b;
      assert_ty_not_equal ~ctxt t0 i;
      assert_ty_not_equal ~ctxt t0 f;
      assert_ty_not_equal ~ctxt t0 hof;
      assert_ty_equal ~ctxt t0 t0;
      assert_ty_not_equal ~ctxt t0 t1;
      assert_ty_not_equal ~ctxt t0 t2
    in
    "Equality" >::: [
      "Unit"      >:: test_unit;
      "Booleans"  >:: test_bool;
      "Integers"  >:: test_int;
      test_fun;
      "Tuples"    >:: test_tuple;
    ]
  in
  "Types" >::: [
    test_constructor;
    test_pp;
    test_equal;
  ]
