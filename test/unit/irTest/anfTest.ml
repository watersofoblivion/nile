open OUnit2
open Nile.Common
open Nile.Ir
open CommonTest

let rec assert_atom_equal ~ctxt expected actual = match expected, actual with
  | Anf.Bool b, Anf.Bool b' ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean literals are not equal" b b'
  | Anf.Int i, Anf.Int i' ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer literals are not equal" i i'
  | Anf.Var idx, Var idx' ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Variables are not equal" idx idx'
  | Anf.Abs (id, arg, res, body), Anf.Abs (id', arg', res', body') ->
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Abstraction variables are not equal" id id';
    TypeTest.assert_type_equal ~ctxt arg arg';
    TypeTest.assert_type_equal ~ctxt res res';
    assert_block_equal ~ctxt body body'
  | expected, actual ->
     assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.atom) ~msg:"Atoms are not equal" expected actual

and assert_expr_equal ~ctxt expected actual = match expected, actual with
  | Anf.UnOp (op, r), Anf.UnOp (op', r') ->
    OpTest.assert_un_equal ~ctxt op op';
    assert_atom_equal ~ctxt r r'
  | Anf.BinOp (l, op, r), Anf.BinOp (l', op', r') ->
    OpTest.assert_bin_equal ~ctxt op op';
    assert_atom_equal ~ctxt l l';
    assert_atom_equal ~ctxt r r'
  | Anf.App (f, x), Anf.App (f', x') ->
    assert_atom_equal ~ctxt f f';
    assert_atom_equal ~ctxt x x'
  | Anf.Atom a, Anf.Atom a' ->
    assert_atom_equal ~ctxt a a'
  | expected, actual ->
     assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.expr) ~msg:"Expressions are not equal" expected actual

and assert_block_equal ~ctxt expected actual = match expected, actual with
  | Anf.Let (b, rest), Anf.Let (b', rest') ->
    assert_binding_equal ~ctxt b b';
    assert_block_equal ~ctxt rest rest'
  | Anf.LetRec (bs, rest), Anf.LetRec (bs', rest') ->
    assert_bindings_equal ~ctxt bs bs';
    assert_block_equal ~ctxt rest rest'
  | Anf.If (c, t, f), Anf.If (c', t', f') ->
    assert_atom_equal ~ctxt c c';
    assert_block_equal ~ctxt t t';
    assert_block_equal ~ctxt f f'
  | Anf.Expr e, Anf.Expr e' ->
    assert_expr_equal ~ctxt e e'
  | expected, actual ->
     assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.block) ~msg:"Blocks are not equal" expected actual

and assert_bindings_equal ~ctxt bs bs' =
  List.iter2 (assert_binding_equal ~ctxt) bs bs'

and assert_binding_equal ~ctxt (id, ty, expr) (id', ty', expr') =
 assert_equal ~ctxt ~printer:string_of_int ~msg:"Bound identifier are not equal" id id';
 TypeTest.assert_type_equal ~ctxt ty ty';
 assert_expr_equal ~ctxt expr expr'

let suite =
  let test_atom =
    let assert_constructor_failure ~ctxt expected actual =
      assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.atom) ~msg:"Atoms are not equal" expected actual
    in

    let test_bool ctxt =
      let assert_bool b =
        let expected = Anf.bool b in
        match expected with
          | Anf.Bool b' -> assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean literals are not equal" b b'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_bool true;
      assert_bool false
    in
    let test_int ctxt =
      let assert_int i =
        let expected = Anf.int i in
        match expected with
          | Anf.Int i' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer literals are not equal" i i'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_int 0;
      assert_int 42;
      assert_int (-42)
    in
    let test_var ctxt =
      let assert_var idx =
        let expected = Anf.var idx in
        match expected with
          | Anf.Var idx' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Variable identifiers are not equal" idx idx'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_var 0;
      assert_var 1
    in
    let test_abs ctxt =
      let assert_abs id arg res body =
        let expected = Anf.abs id arg res body in
        match expected with
          | Anf.Abs (id', arg', res', body') ->
            assert_equal ~ctxt ~printer:string_of_int ~msg:"Bound variable identifiers are not equal" id id';
            TypeTest.assert_type_equal ~ctxt arg arg';
            TypeTest.assert_type_equal ~ctxt res res';
            assert_block_equal ~ctxt body body'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      Anf.var 0
        |> Anf.atom
        |> Anf.expr
        |> assert_abs 0 Type.int Type.bool
    in
    "Atoms" >::: [
      "Boolean"              >:: test_bool;
      "Integer"              >:: test_int;
      "Variable"             >:: test_var;
      "Function Abstraction" >:: test_abs;
    ]
  in
  let test_expr =
    let assert_constructor_failure ~ctxt expected actual =
      assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.expr) ~msg:"Expressions are not equal" expected actual
    in

    let test_un_op ctxt =
      let assert_un_op op r =
        let expected = Anf.un_op op r in
        match expected with
          | Anf.UnOp (op', r') ->
            OpTest.assert_un_equal ~ctxt op op';
            assert_atom_equal ~ctxt r r'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_un_op Op.un_not (Anf.bool true)
    in
    let test_bin_op ctxt =
      let assert_bin_op l op r =
        let expected = Anf.bin_op l op r in
        match expected with
          | Anf.BinOp (l', op', r') ->
            OpTest.assert_bin_equal ~ctxt op op';
            assert_atom_equal ~ctxt l l';
            assert_atom_equal ~ctxt r r'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_bin_op (Anf.int 1) Op.bin_eq (Anf.int 2)
    in
    let test_app ctxt =
      let assert_app f x =
        let expected = Anf.app f x in
        match expected with
          | Anf.App (f', x') ->
            assert_atom_equal ~ctxt f f';
            assert_atom_equal ~ctxt x x'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_app (Anf.var 0) (Anf.var 1)
    in
    let test_atom ctxt =
      let assert_atom a =
        let expected = Anf.atom a in
        match expected with
          | Anf.Atom a' -> assert_atom_equal ~ctxt a a'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      assert_atom (Anf.int 1)
    in
    "Expressions" >::: [
      "Unary Operator"       >:: test_un_op;
      "Binary Operator"      >:: test_bin_op;
      "Function Application" >:: test_app;
      "Atom"                 >:: test_atom;
    ]
  in
  let test_block =
    let assert_constructor_failure ~ctxt expected actual =
      assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Pretty.block) ~msg:"Blocks are not equal" expected actual
    in

    let test_bind ctxt =
      let assert_bind b rest =
        let expected = Anf.bind b rest in
        match expected with
          | Anf.Let (b', rest') ->
            assert_binding_equal ~ctxt b b';
            assert_block_equal ~ctxt rest rest'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      let b =
        Anf.un_op Op.un_not (Anf.bool true)
          |> Anf.binding 0 Type.bool
      in
      Anf.var 0
        |> Anf.atom
        |> Anf.expr
        |> assert_bind b
    in
    let test_bind_rec ctxt =
      let assert_bind_rec bs rest =
        let expected = Anf.bind_rec bs rest in
        match expected with
          | Anf.LetRec (bs', rest') ->
            assert_bindings_equal ~ctxt bs bs';
            assert_block_equal ~ctxt rest rest'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      let bs = [
        Anf.int 1
          |> Anf.atom
          |> Anf.binding 0 Type.int;
        Anf.int 2
          |> Anf.atom
          |> Anf.binding 1 Type.int
      ] in
      Anf.bin_op (Anf.var 0) Op.bin_eq (Anf.var 1)
        |> Anf.expr
        |> assert_bind_rec bs
    in
    let test_cond ctxt =
      let assert_cond c t f =
        let expected = Anf.cond c t f in
        match expected with
          | Anf.If (c', t', f') ->
            assert_atom_equal ~ctxt c c';
            assert_block_equal ~ctxt t t';
            assert_block_equal ~ctxt f f'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      let c = Anf.bool true in
      let t =
        Anf.int 1
          |> Anf.atom
          |> Anf.expr
      in
      let f =
        Anf.int 2
          |> Anf.atom
          |> Anf.expr
      in
      assert_cond c t f
    in
    let test_expr ctxt =
      let assert_expr e =
        let expected = Anf.expr e in
        match expected with
          | Anf.Expr e' -> assert_expr_equal ~ctxt e e'
          | actual -> assert_constructor_failure ~ctxt expected actual
      in

      Anf.int 1
        |> Anf.atom
        |> assert_expr
    in
    "Block" >::: [
      "Value Binding"            >:: test_bind;
      "Recursive Value Bindings" >:: test_bind_rec;
      "Conditional"              >:: test_cond;
      "Expression"               >:: test_expr;
    ]
  in
  "Administrative Normal Form" >::: [
    test_atom;
    test_expr;
    test_block;
  ]
