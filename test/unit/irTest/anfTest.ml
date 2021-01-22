open OUnit2
open Nile.Common
open Nile.Ir
open CommonTest

(* Assertions *)

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
     assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_atom) ~msg:"Atoms are not equal" expected actual

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
     assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_expr) ~msg:"Expressions are not equal" expected actual

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
     assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_block) ~msg:"Blocks are not equal" expected actual

and assert_bindings_equal ~ctxt bs bs' =
  List.iter2 (assert_binding_equal ~ctxt) bs bs'

and assert_binding_equal ~ctxt (id, ty, expr) (id', ty', expr') =
 assert_equal ~ctxt ~printer:string_of_int ~msg:"Bound identifier are not equal" id id';
 TypeTest.assert_type_equal ~ctxt ty ty';
 assert_expr_equal ~ctxt expr expr'

let assert_top_equal ~ctxt expected actual = match expected, actual with
  | Anf.TopLet b, Anf.TopLet b' -> assert_binding_equal ~ctxt b b'
  | Anf.TopRec bs, Anf.TopRec bs' -> assert_bindings_equal ~ctxt bs bs'
  | expected, actual ->
    assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_top) ~msg:"Top-level statments are not equal" expected actual

let assert_file_equal ~ctxt expected actual =
  let iter2 = assert_top_equal ~ctxt in
  List.iter2 iter2 expected actual

(* Constructors *)

let assert_atom_constructor_failure ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_atom) ~msg:"Atoms are not equal" expected actual

let test_bool ctxt =
  let assert_bool b =
    let expected = Anf.bool b in
    match expected with
      | Anf.Bool b' -> assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean literals are not equal" b b'
      | actual -> assert_atom_constructor_failure ~ctxt expected actual
  in
  assert_bool true;
  assert_bool false

let test_int ctxt =
  let assert_int i =
    let expected = Anf.int i in
    match expected with
      | Anf.Int i' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer literals are not equal" i i'
      | actual -> assert_atom_constructor_failure ~ctxt expected actual
  in
  assert_int 0;
  assert_int 42;
  assert_int (-42)

let test_var ctxt =
  let assert_var idx =
    let expected = Anf.var idx in
    match expected with
      | Anf.Var idx' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Variable identifiers are not equal" idx idx'
      | actual -> assert_atom_constructor_failure ~ctxt expected actual
  in
  assert_var 0;
  assert_var 1

let test_abs ctxt =
  let assert_abs id arg res body =
    let expected = Anf.abs id arg res body in
    match expected with
      | Anf.Abs (id', arg', res', body') ->
        assert_equal ~ctxt ~printer:string_of_int ~msg:"Bound variable identifiers are not equal" id id';
        TypeTest.assert_type_equal ~ctxt arg arg';
        TypeTest.assert_type_equal ~ctxt res res';
        assert_block_equal ~ctxt body body'
      | actual -> assert_atom_constructor_failure ~ctxt expected actual
  in
  Anf.var 0
    |> Anf.atom
    |> Anf.expr
    |> assert_abs 0 Type.int Type.bool

let assert_expr_constructor_failure ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_expr) ~msg:"Expressions are not equal" expected actual

let test_un_op ctxt =
  let assert_un_op op r =
    let expected = Anf.un_op op r in
    match expected with
      | Anf.UnOp (op', r') ->
        OpTest.assert_un_equal ~ctxt op op';
        assert_atom_equal ~ctxt r r'
      | actual -> assert_expr_constructor_failure ~ctxt expected actual
  in
  assert_un_op Op.un_not (Anf.bool true)

let test_bin_op ctxt =
  let assert_bin_op l op r =
    let expected = Anf.bin_op l op r in
    match expected with
      | Anf.BinOp (l', op', r') ->
        OpTest.assert_bin_equal ~ctxt op op';
        assert_atom_equal ~ctxt l l';
        assert_atom_equal ~ctxt r r'
      | actual -> assert_expr_constructor_failure ~ctxt expected actual
  in
  assert_bin_op (Anf.int 1) Op.bin_eq (Anf.int 2)

let test_app ctxt =
  let assert_app f x =
    let expected = Anf.app f x in
    match expected with
      | Anf.App (f', x') ->
        assert_atom_equal ~ctxt f f';
        assert_atom_equal ~ctxt x x'
      | actual -> assert_expr_constructor_failure ~ctxt expected actual
  in
  assert_app (Anf.var 0) (Anf.var 1)

let test_atom ctxt =
  let assert_atom a =
    let expected = Anf.atom a in
    match expected with
      | Anf.Atom a' -> assert_atom_equal ~ctxt a a'
      | actual -> assert_expr_constructor_failure ~ctxt expected actual
  in
  assert_atom (Anf.int 1)

let assert_block_constructor_failure ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_block) ~msg:"Blocks are not equal" expected actual

let test_bind ctxt =
  let assert_bind b rest =
    let expected = Anf.bind b rest in
    match expected with
      | Anf.Let (b', rest') ->
        assert_binding_equal ~ctxt b b';
        assert_block_equal ~ctxt rest rest'
      | actual -> assert_block_constructor_failure ~ctxt expected actual
  in
  let b =
    Anf.un_op Op.un_not (Anf.bool true)
      |> Anf.binding 0 Type.bool
  in
  Anf.var 0
    |> Anf.atom
    |> Anf.expr
    |> assert_bind b

let test_bind_rec ctxt =
  let assert_bind_rec bs rest =
    let expected = Anf.bind_rec bs rest in
    match expected with
      | Anf.LetRec (bs', rest') ->
        assert_bindings_equal ~ctxt bs bs';
        assert_block_equal ~ctxt rest rest'
      | actual -> assert_block_constructor_failure ~ctxt expected actual
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

let test_cond ctxt =
  let assert_cond c t f =
    let expected = Anf.cond c t f in
    match expected with
      | Anf.If (c', t', f') ->
        assert_atom_equal ~ctxt c c';
        assert_block_equal ~ctxt t t';
        assert_block_equal ~ctxt f f'
      | actual -> assert_block_constructor_failure ~ctxt expected actual
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

let test_expr ctxt =
  let assert_expr e =
    let expected = Anf.expr e in
    match expected with
      | Anf.Expr e' -> assert_expr_equal ~ctxt e e'
      | actual -> assert_block_constructor_failure ~ctxt expected actual
  in
  Anf.int 1
    |> Anf.atom
    |> assert_expr

let assert_top_constructor_failure ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Anf.pp_top) ~msg:"Top-level statements are not equal" expected actual

let test_top_bind ctxt =
  let assert_top_bind b =
    let expected = Anf.top_bind b in
    match expected with
      | Anf.TopLet b' ->
        assert_binding_equal ~ctxt b b'
      | actual -> assert_top_constructor_failure ~ctxt expected actual
  in
  Anf.un_op Op.un_not (Anf.bool true)
    |> Anf.binding 0 Type.bool
    |> assert_top_bind

let test_top_bind_rec ctxt =
  let assert_top_bind_rec bs =
    let expected = Anf.top_bind_rec bs in
    match expected with
      | Anf.TopRec bs' ->
        assert_bindings_equal ~ctxt bs bs';
      | actual -> assert_top_constructor_failure ~ctxt expected actual
  in
  [Anf.int 1
    |> Anf.atom
    |> Anf.binding 0 Type.int;
  Anf.int 2
    |> Anf.atom
    |> Anf.binding 1 Type.int]
  |> assert_top_bind_rec

let test_file ctxt =
  let assert_file tops =
    let actual = Anf.file tops in
    let iter2 = assert_top_equal ~ctxt in
    List.iter2 iter2 tops actual
  in
  [Anf.un_op Op.un_not (Anf.bool true)
      |> Anf.binding 0 Type.bool
      |> Anf.top_bind;
   [Anf.int 1
      |> Anf.atom
      |> Anf.binding 0 Type.int;
    Anf.int 2
      |> Anf.atom
      |> Anf.binding 1 Type.int]
    |> Anf.top_bind_rec]
  |> assert_file

let test_constructor =
  "Constructors" >::: [
    "Atoms" >::: [
      "Boolean"              >:: test_bool;
      "Integer"              >:: test_int;
      "Variable"             >:: test_var;
      "Function Abstraction" >:: test_abs;
    ];
    "Expressions" >::: [
      "Unary Operator"       >:: test_un_op;
      "Binary Operator"      >:: test_bin_op;
      "Function Application" >:: test_app;
      "Atom"                 >:: test_atom;
    ];
    "Blocks" >::: [
      "Value Binding"            >:: test_bind;
      "Recursive Value Bindings" >:: test_bind_rec;
      "Conditional"              >:: test_cond;
      "Expression"               >:: test_expr;
    ];
    "Top-Level Statements" >::: [
      "Value Binding"            >:: test_top_bind;
      "Recursive Value Bindings" >:: test_top_bind_rec;
    ];
    "Files" >:: test_file;
  ]

(* A-Normalization *)

let test_normalize =
  "A-Normalization" >::: [

  ]

(* Pretty Printing *)

let test_pp =
  "Pretty Printing" >::: [

  ]

(* Type Checking *)

let assert_type_of_atom ~ctxt ?env:(env = Anf.env) expected atom =
  Anf.type_of_atom env atom
    |> TypeTest.assert_type_equal ~ctxt expected

let test_type_of_bool ctxt =
  Anf.bool true
    |> assert_type_of_atom ~ctxt Type.bool;
  Anf.bool false
    |> assert_type_of_atom ~ctxt Type.bool

let test_type_of_int ctxt =
  Anf.int 42
    |> assert_type_of_atom ~ctxt Type.int

let test_type_of_bound_var ctxt =
  let ty = Type.int in
  let env =
    Anf.env
  in
  Anf.var 0
    |> assert_type_of_atom ~ctxt ~env ty

let test_type_of_unbound_var ctxt =
  Anf.var 0
    |> assert_type_of_atom ~ctxt Type.int

let test_type_of_abs ctxt =
  Anf.bin_op (Anf.var 0) Op.bin_eq (Anf.var 0)
    |> Anf.expr
    |> Anf.abs 0 Type.int Type.bool
    |> assert_type_of_atom ~ctxt (Type.func Type.int Type.bool)

let test_type_of_abs_declaration_mismatch ctxt =
  Anf.var 0
    |> Anf.atom
    |> Anf.expr
    |> Anf.abs 0 Type.int Type.bool
    |> assert_type_of_atom ~ctxt (Type.func Type.int Type.int)

let test_type_of =
  "Type Checking" >::: [
    "Atoms" >::: [
      "Boolean"  >:: test_type_of_bool;
      "Integer"  >:: test_type_of_int;
      "Variable" >::: [
        "Bound"   >:: test_type_of_bound_var;
        "Unbound" >:: test_type_of_unbound_var;
      ];
      "Function Abstraction" >::: [
        "Valid"                >:: test_type_of_abs;
        "Declaration Mismatch" >:: test_type_of_abs_declaration_mismatch;
      ];
    ];
  ]

let suite =
  "Administrative Normal Form" >::: [
    test_constructor;
    test_normalize;
    test_pp;
    test_type_of;
  ]
