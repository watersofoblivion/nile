open OUnit2
open Nile.Common
open Nile.Syntax
open CommonTest

let rec assert_expr_equal ~ctxt expected actual = match expected, actual with
  | Ast.Bool (loc, b), Ast.Bool (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean literals are not equal"
  | Ast.Int (loc, i), Ast.Int (loc', i') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Integer literals are not equal"
  | Ast.UnOp (loc, op, r), Ast.UnOp (loc', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    OpTest.assert_un_equal ~ctxt op op';
    assert_expr_equal ~ctxt r r'
  | Ast.BinOp (loc, l, op, r), Ast.BinOp (loc', l', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    OpTest.assert_bin_equal ~ctxt op op';
    assert_expr_equal ~ctxt l l';
    assert_expr_equal ~ctxt r r'
  | Ast.Let (loc, b, rest), Ast.Let (loc', b', rest') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b';
    assert_expr_equal ~ctxt rest rest'
  | Ast.LetRec (loc, bs, rest), Ast.LetRec (loc', bs', rest') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs';
    assert_expr_equal ~ctxt rest rest'
  | Ast.Abs (loc, id, ty, res, expr), Ast.Abs (loc', id', ty', res', expr') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~msg:"Parameter identifiers are not equal" id id';
    TypeTest.assert_type_equal ~ctxt ty ty';
    TypeTest.assert_type_equal ~ctxt res res';
    assert_expr_equal ~ctxt expr expr'
  | Ast.App (loc, f, x), Ast.App (loc', f', x') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt f f';
    assert_expr_equal ~ctxt x x'
  | Ast.Var (loc, id), Ast.Var (loc', id') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Variable identifiers are not equal" id id'
  | Ast.If (loc, c, t, f), Ast.If (loc', c', t', f') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt c c';
    assert_expr_equal ~ctxt t t';
    assert_expr_equal ~ctxt f f'
  | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.expr) ~msg:"Expressions are not equal" expected actual
and assert_bindings_equal ~ctxt bs bs' = List.iter2 (assert_binding_equal ~ctxt) bs bs'
and assert_binding_equal ~ctxt (loc, id, ty, expr) (loc', id', ty', expr') =
  LocTest.assert_loc_equal ~ctxt loc loc';
  assert_equal ~ctxt ~printer:Fun.id ~msg:"Bound identifier are not equal" id id';
  TypeTest.assert_type_equal ~ctxt ty ty';
  assert_expr_equal ~ctxt expr expr'

let assert_top_equal ~ctxt expected actual = match expected, actual with
  | Ast.TopLet (loc, b), Ast.TopLet (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b'
  | Ast.TopRec (loc, bs), Ast.TopRec (loc', bs') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs'
  | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.top) ~msg:"Top-level bindings are not equal" expected actual

let assert_file_equal ~ctxt expected actual = List.iter2 (assert_top_equal ~ctxt) expected actual

let suite =
  let test_expr =
    let test_constructor =
      let assert_constructor_failure ~ctxt expected actual =
        assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.expr) ~msg:"Expressions are not equal" expected actual
      in

      let test_primitive =
        let test_bool ctxt =
          let assert_bool b =
            let loc = LocTest.gen () in
            let expected = Ast.bool loc b in
            match expected with
              | Ast.Bool (loc', b') ->
                LocTest.assert_loc_equal ~ctxt loc loc';
                assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean values are not equal"
              | actual -> assert_constructor_failure ~ctxt expected actual
          in

          assert_bool true;
          assert_bool false
        in
        let test_int ctxt =
          let assert_int i =
            let loc = LocTest.gen () in
            let expected = Ast.int loc i in
            match expected with
              | Ast.Int (loc', i') ->
                LocTest.assert_loc_equal ~ctxt loc loc';
                assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer values are not equal" i i'
              | actual -> assert_constructor_failure ~ctxt expected actual
          in

          assert_int 0;
          assert_int 42;
          assert_int (-42)
        in
        let test_var ctxt =
          let assert_var id =
            let loc = LocTest.gen () in
            let expected = Ast.var loc id in
            match expected with
              | Ast.Var (loc', id') ->
                LocTest.assert_loc_equal ~ctxt loc loc';
                assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Variable names are not equal"
              | actual -> assert_constructor_failure ~ctxt expected actual
          in

          assert_var "f";
          assert_var "x"
        in
        "Primitives" >::: [
          "Booleans"  >:: test_bool;
          "Integers"  >:: test_int;
          "Variables" >:: test_var;
        ]
      in
      let test_operator =
        let test_un_op =
          let assert_un_op ~ctxt op =
            let loc = LocTest.gen () in
            let r = Ast.int LocTest.dummy 1 in
            let expected = Ast.un_op loc op r in
            match expected with
              | Ast.UnOp (loc', op', r') ->
                LocTest.assert_loc_equal ~ctxt loc loc';
                OpTest.assert_un_equal ~ctxt op op';
                assert_expr_equal ~ctxt r r'
              | actual -> assert_constructor_failure ~ctxt expected actual
          in

          let test_not ctxt = assert_un_op ~ctxt Op.un_not in
          "Unary Operators" >::: [
            "Boolean Negation" >:: test_not;
          ]
        in
        let test_bin_op =
          let assert_bin_op ~ctxt op =
            let loc = LocTest.gen () in
            let l = Ast.int LocTest.dummy 1 in
            let r = Ast.int LocTest.dummy 2 in
            let expected = Ast.bin_op loc l op r in
            match expected with
              | Ast.BinOp (loc', l', op', r') ->
                LocTest.assert_loc_equal ~ctxt loc loc';
                OpTest.assert_bin_equal ~ctxt op op';
                assert_expr_equal ~ctxt l l';
                assert_expr_equal ~ctxt r r'
              | actual -> assert_constructor_failure ~ctxt expected actual
          in

          let test_add ctxt = assert_bin_op ~ctxt Op.bin_add in
          let test_sub ctxt = assert_bin_op ~ctxt Op.bin_sub in
          let test_mul ctxt = assert_bin_op ~ctxt Op.bin_mul in
          let test_div ctxt = assert_bin_op ~ctxt Op.bin_div in
          let test_mod ctxt = assert_bin_op ~ctxt Op.bin_mod in
          let test_and ctxt = assert_bin_op ~ctxt Op.bin_and in
          let test_or  ctxt = assert_bin_op ~ctxt Op.bin_or  in
          let test_eq  ctxt = assert_bin_op ~ctxt Op.bin_eq  in
          let test_neq ctxt = assert_bin_op ~ctxt Op.bin_neq in
          let test_lte ctxt = assert_bin_op ~ctxt Op.bin_lte in
          let test_lt  ctxt = assert_bin_op ~ctxt Op.bin_lt  in
          let test_gt  ctxt = assert_bin_op ~ctxt Op.bin_gt  in
          let test_gte ctxt = assert_bin_op ~ctxt Op.bin_gte in
          "Binary Operators" >::: [
            "Addition"              >:: test_add;
            "Subtration"            >:: test_sub;
            "Multiplication"        >:: test_mul;
            "Integer Division"      >:: test_div;
            "Modulus"               >:: test_mod;
            "Logical And"           >:: test_and;
            "Logical Or"            >:: test_or;
            "Equality"              >:: test_eq;
            "Inequality"            >:: test_neq;
            "Less Than or Equal"    >:: test_lte;
            "Less Than"             >:: test_lt;
            "Greater Than"          >:: test_gt;
            "Greater Than or Equal" >:: test_gte;
          ]
        in
        "Operators" >::: [
          test_un_op;
          test_bin_op;
        ]
      in
      let test_bind =
        "Value Binding" >::: [

        ]
      in
      let test_bind_rec =
        "Recursive Value Bindings" >::: [

        ]
      in
      let test_abs =
        "Function Abstraction" >::: [

        ]
      in
      let test_app =
        "Function Application" >::: [

        ]
      in
      let test_cond =
        "Conditional" >::: [

        ]
      in
      "Constructors" >::: [
        test_primitive;
        test_operator;
        test_cond;
        test_bind;
        test_bind_rec;
        test_abs;
        test_app;
      ]
    in
    let test_loc =
      let loc = Loc.mock "-" (1, 2, 3) (4, 5, 6) in
      let assert_loc ~ctxt loc ast =
        ast
          |> Ast.loc_expr
          |> LocTest.assert_loc_equal ~ctxt loc
      in

      let test_bool ctxt =
        Ast.bool loc true
          |> assert_loc ~ctxt loc
      in
      let test_int ctxt =
        Ast.int loc 1
          |> assert_loc ~ctxt loc
      in
      let test_var ctxt =
        Ast.var loc "x"
          |> assert_loc ~ctxt loc
      in
      let test_un_op ctxt =
        Ast.bool LocTest.dummy true
          |> Ast.un_op loc Op.un_not
          |> assert_loc ~ctxt loc
      in
      let test_bin_op ctxt =
        let l = Ast.int LocTest.dummy 1 in
        let r = Ast.int LocTest.dummy 2 in
        Ast.bin_op loc l Op.bin_add r
          |> assert_loc ~ctxt loc
      in
      let test_cond ctxt =
        let c = Ast.bool LocTest.dummy true in
        let t = Ast.int LocTest.dummy 1 in
        let f = Ast.int LocTest.dummy 2 in
        Ast.cond loc c t f
          |> assert_loc ~ctxt loc
      in
      let test_abs ctxt =
        Ast.int LocTest.dummy 1
          |> Ast.abs loc "param" Type.int Type.int
          |> assert_loc ~ctxt loc
      in
      let test_app ctxt =
        let f = Ast.var LocTest.dummy "f" in
        Ast.var LocTest.dummy "x"
          |> Ast.app loc f
          |> assert_loc ~ctxt loc
      in
      let test_bind ctxt =
        let b =
          Ast.int LocTest.dummy 1
            |> Ast.binding LocTest.dummy "x" Type.int
        in
        Ast.var LocTest.dummy "x"
          |> Ast.bind loc b
          |> assert_loc ~ctxt loc
      in
      let test_bind_rec ctxt =
        let bs = [
          Ast.int LocTest.dummy 1
            |> Ast.binding LocTest.dummy "x" Type.int;
          Ast.int LocTest.dummy 2
            |> Ast.binding LocTest.dummy "y" Type.int
        ] in
        let x = Ast.var LocTest.dummy "x" in
        let y = Ast.var LocTest.dummy "y" in
        Ast.bin_op LocTest.dummy x Op.bin_eq y
          |> Ast.bind_rec loc bs
          |> assert_loc ~ctxt loc
      in
      "Location Information" >::: [
        "Boolean"              >:: test_bool;
        "Integer"              >:: test_int;
        "Variable"             >:: test_var;
        "Unary Operator"       >:: test_un_op;
        "Binary Operator"      >:: test_bin_op;
        "Conditional"          >:: test_cond;
        "Function Abstraction" >:: test_abs;
        "Function Application" >:: test_app;
        "Bindings"             >:: test_bind;
        "Recursive Bindings"   >:: test_bind_rec;
      ]
    in
    "Expressions" >::: [
      test_constructor;
      test_loc;
    ]
  in
  let test_top =
    let loc = Loc.mock "-" (1, 2, 3) (4, 5, 6) in

    let id = "id-one" in
    let ty = Type.int in
    let x = Ast.int LocTest.dummy 1 in
    let b = Ast.binding LocTest.dummy id ty x in

    let id' = "id-two" in
    let ty' = Type.bool in
    let x' = Ast.bool LocTest.dummy true in
    let b' = Ast.binding LocTest.dummy id' ty' x' in

    let test_constructor =
      let test_top_bind ctxt =
        let top = Ast.top_bind loc b in
        match top with
          | Ast.TopLet (loc', b') ->
            LocTest.assert_loc_equal ~ctxt loc loc';
            assert_binding_equal ~ctxt b b'
          | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Pretty.top) ~msg:"Top-level bindings are not equal" top top'
      in
      let test_top_bind_rec ctxt =
        let bs = [b; b'] in
        let top = Ast.top_bind_rec loc bs in
        match top with
          | Ast.TopRec (loc', bs') ->
            LocTest.assert_loc_equal ~ctxt loc loc';
            assert_bindings_equal ~ctxt bs bs'
          | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Pretty.top) ~msg:"Top-level bindings are not equal" top top'
      in
      "Constructors" >::: [
        "Value Binding"           >:: test_top_bind;
        "Recursive Value Binding" >:: test_top_bind_rec;
      ]
    in
    let test_loc =
      let test_top_bind ctxt =
        Ast.top_bind loc b
          |> Ast.loc_top
          |> LocTest.assert_loc_equal ~ctxt loc
      in
      let test_top_bind_rec ctxt =
        Ast.top_bind_rec loc [b; b']
          |> Ast.loc_top
          |> LocTest.assert_loc_equal ~ctxt loc
      in
      "Location Information" >::: [
        "Value Binding"           >:: test_top_bind;
        "Recursive Value Binding" >:: test_top_bind_rec;
      ]
    in
    "Top-Level Bindings" >::: [
      test_constructor;
      test_loc;
    ]
  in
  let test_file =
    let expected =
      let b =
        let ty = Type.int in
        Ast.int LocTest.dummy 1
          |> Ast.binding LocTest.dummy "id-one" ty
      in
      let b' =
        let ty = Type.bool in
        Ast.bool LocTest.dummy true
          |> Ast.binding LocTest.dummy "id-two" ty
      in
      Ast.top_bind_rec LocTest.dummy [b; b']
    in
    let expected' =
      let ty = Type.bool in
      Ast.bool LocTest.dummy true
        |> Ast.binding LocTest.dummy "id-two" ty
        |> Ast.top_bind LocTest.dummy
    in
    let file = Ast.file [expected; expected'] in

    let test_constructor =
      let test_file ctxt = match file with
        | actual :: actual' :: [] ->
          assert_top_equal ~ctxt expected actual;
          assert_top_equal ~ctxt expected' actual'
        | actual -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Pretty.file) ~msg:"Source files are not equal" file actual
      in
      "Constructor" >:: test_file;
    in
    "Source Files" >::: [
      test_constructor;
    ]
  in
  "Abstract Syntax" >::: [
    test_expr;
    test_top;
    test_file;
  ]
