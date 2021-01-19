open Format
open OUnit2
open Nile.Common
open Nile.Syntax
open CommonTest

(* Assertions *)

let rec assert_expr_equal ~ctxt expected actual = match expected, actual with
  | Annot.Bool (loc, b), Annot.Bool (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean literals are not equal"
  | Annot.Int (loc, i), Annot.Int (loc', i') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Integer literals are not equal"
  | Annot.UnOp (loc, op, r), Annot.UnOp (loc', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    OpTest.assert_un_equal ~ctxt op op';
    assert_expr_equal ~ctxt r r'
  | Annot.BinOp (loc, l, op, r), Annot.BinOp (loc', l', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    OpTest.assert_bin_equal ~ctxt op op';
    assert_expr_equal ~ctxt l l';
    assert_expr_equal ~ctxt r r'
  | Annot.Let (loc, b, rest), Annot.Let (loc', b', rest') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b';
    assert_expr_equal ~ctxt rest rest'
  | Annot.LetRec (loc, bs, rest), Annot.LetRec (loc', bs', rest') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs';
    assert_expr_equal ~ctxt rest rest'
  | Annot.Abs (loc, id, ty, res, expr), Annot.Abs (loc', id', ty', res', expr') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~msg:"Parameter identifiers are not equal" id id';
    TypeTest.assert_type_equal ~ctxt ty ty';
    TypeTest.assert_type_equal ~ctxt res res';
    assert_expr_equal ~ctxt expr expr'
  | Annot.App (loc, f, x), Annot.App (loc', f', x') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt f f';
    assert_expr_equal ~ctxt x x'
  | Annot.Var (loc, id), Annot.Var (loc', id') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Variable identifiers are not equal" id id'
  | Annot.If (loc, c, t, f), Annot.If (loc', c', t', f') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt c c';
    assert_expr_equal ~ctxt t t';
    assert_expr_equal ~ctxt f f'
  | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Annot.pp_expr) ~msg:"Expressions are not equal" expected actual
and assert_bindings_equal ~ctxt bs bs' = List.iter2 (assert_binding_equal ~ctxt) bs bs'
and assert_binding_equal ~ctxt (loc, id, ty, expr) (loc', id', ty', expr') =
  LocTest.assert_loc_equal ~ctxt loc loc';
  assert_equal ~ctxt ~printer:Fun.id ~msg:"Bound identifier are not equal" id id';
  TypeTest.assert_type_equal ~ctxt ty ty';
  assert_expr_equal ~ctxt expr expr'

let assert_top_equal ~ctxt expected actual = match expected, actual with
  | Annot.TopLet (loc, b), Annot.TopLet (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b'
  | Annot.TopRec (loc, bs), Annot.TopRec (loc', bs') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs'
  | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Annot.pp_top) ~msg:"Top-level bindings are not equal" expected actual

let assert_file_equal ~ctxt expected actual = List.iter2 (assert_top_equal ~ctxt) expected actual

(* Constructors *)

let assert_constructor_failure ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Annot.pp_expr) ~msg:"Expressions are not equal" expected actual

let test_bool ctxt =
  let assert_bool b =
    let loc = LocTest.gen () in
    let expected = Annot.bool loc b in
    match expected with
      | Annot.Bool (loc', b') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean values are not equal"
      | actual -> assert_constructor_failure ~ctxt expected actual
  in
  assert_bool true;
  assert_bool false

let test_int ctxt =
  let assert_int i =
    let loc = LocTest.gen () in
    let expected = Annot.int loc i in
    match expected with
      | Annot.Int (loc', i') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer values are not equal" i i'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in
  assert_int 0;
  assert_int 42;
  assert_int (-42)

let test_var ctxt =
  let assert_var id =
    let loc = LocTest.gen () in
    let expected = Annot.var loc id in
    match expected with
      | Annot.Var (loc', id') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Variable names are not equal"
      | actual -> assert_constructor_failure ~ctxt expected actual
  in
  assert_var "f";
  assert_var "x"

let assert_un_op ~ctxt op =
  let loc = LocTest.gen () in
  let r = Annot.int LocTest.dummy 1 in
  let expected = Annot.un_op loc op r in
  match expected with
    | Annot.UnOp (loc', op', r') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      OpTest.assert_un_equal ~ctxt op op';
      assert_expr_equal ~ctxt r r'
    | actual -> assert_constructor_failure ~ctxt expected actual

let test_not ctxt = assert_un_op ~ctxt Op.un_not

let assert_bin_op ~ctxt op =
  let loc = LocTest.gen () in
  let l = Annot.int LocTest.dummy 1 in
  let r = Annot.int LocTest.dummy 2 in
  let expected = Annot.bin_op loc l op r in
  match expected with
    | Annot.BinOp (loc', l', op', r') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      OpTest.assert_bin_equal ~ctxt op op';
      assert_expr_equal ~ctxt l l';
      assert_expr_equal ~ctxt r r'
    | actual -> assert_constructor_failure ~ctxt expected actual

let test_add ctxt = assert_bin_op ~ctxt Op.bin_add
let test_sub ctxt = assert_bin_op ~ctxt Op.bin_sub
let test_mul ctxt = assert_bin_op ~ctxt Op.bin_mul
let test_div ctxt = assert_bin_op ~ctxt Op.bin_div
let test_mod ctxt = assert_bin_op ~ctxt Op.bin_mod
let test_and ctxt = assert_bin_op ~ctxt Op.bin_and
let test_or  ctxt = assert_bin_op ~ctxt Op.bin_or
let test_eq  ctxt = assert_bin_op ~ctxt Op.bin_eq
let test_neq ctxt = assert_bin_op ~ctxt Op.bin_neq
let test_lte ctxt = assert_bin_op ~ctxt Op.bin_lte
let test_lt  ctxt = assert_bin_op ~ctxt Op.bin_lt
let test_gt  ctxt = assert_bin_op ~ctxt Op.bin_gt
let test_gte ctxt = assert_bin_op ~ctxt Op.bin_gte

let test_cond ctxt =
  let assert_cond c t f =
    let loc = LocTest.gen () in
    let expected = Annot.cond loc c t f in
    match expected with
      | Annot.If (loc', c', t', f') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_expr_equal ~ctxt c c';
        assert_expr_equal ~ctxt t t';
        assert_expr_equal ~ctxt f f'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let c = Annot.bool LocTest.dummy true in
  let t = Annot.int LocTest.dummy 1 in
  let f = Annot.int LocTest.dummy 2 in
  assert_cond c t f

let id = "id-one"
let ty = Type.int
let x = Annot.int LocTest.dummy 1
let b = Annot.binding LocTest.dummy id ty x

let id' = "id-two"
let ty' = Type.bool
let x' = Annot.bool LocTest.dummy true
let b' = Annot.binding LocTest.dummy id' ty' x'

let test_bind ctxt =
  let assert_bind b rest =
    let loc = LocTest.gen () in
    let expected = Annot.bind loc b rest in
    match expected with
      | Annot.Let (loc', b', rest') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_binding_equal ~ctxt b b';
        assert_expr_equal ~ctxt rest rest'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  Annot.var LocTest.dummy id
    |> assert_bind b

let test_bind_rec ctxt =
  let assert_bind bs rest =
    let loc = LocTest.gen () in
    let expected = Annot.bind_rec loc bs rest in
    match expected with
      | Annot.LetRec (loc', bs', rest') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_bindings_equal ~ctxt bs bs';
        assert_expr_equal ~ctxt rest rest'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let l = Annot.var LocTest.dummy id in
  let r = Annot.var LocTest.dummy id' in
  Annot.bin_op LocTest.dummy l Op.bin_eq r
    |> assert_bind [b; b']

let test_abs ctxt =
  let assert_abs id ty res body =
    let loc = LocTest.gen () in
    let expected = Annot.abs loc id ty res body in
    match expected with
      | Annot.Abs (loc', id', ty', res', body') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:Fun.id ~msg:"Bound function identifiers are not equal" id id';
        TypeTest.assert_type_equal ~ctxt ty ty';
        TypeTest.assert_type_equal ~ctxt res res';
        assert_expr_equal ~ctxt body body'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let var = Annot.var LocTest.dummy id in
  Annot.bin_op LocTest.dummy var Op.bin_eq var
    |> assert_abs id ty ty'

let test_app ctxt =
  let assert_app f x =
    let loc = LocTest.gen () in
    let expected = Annot.app loc f x in
    match expected with
      | Annot.App (loc', f', x') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_expr_equal ~ctxt f f';
        assert_expr_equal ~ctxt x x'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let f = Annot.var LocTest.dummy id in
  Annot.var LocTest.dummy id'
    |> assert_app f

let test_top_bind ctxt =
  let loc = LocTest.gen () in
  let top = Annot.top_bind loc b in
  match top with
    | Annot.TopLet (loc', b') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      assert_binding_equal ~ctxt b b'
    | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Annot.pp_top) ~msg:"Top-level bindings are not equal" top top'

let test_top_bind_rec ctxt =
  let loc = LocTest.gen () in
  let bs = [b; b'] in
  let top = Annot.top_bind_rec loc bs in
  match top with
    | Annot.TopRec (loc', bs') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      assert_bindings_equal ~ctxt bs bs'
    | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Annot.pp_top) ~msg:"Top-level bindings are not equal" top top'

let test_file ctxt =
  let expected =
    let b =
      let ty = Type.int in
      Annot.int LocTest.dummy 1
        |> Annot.binding LocTest.dummy "id-one" ty
    in
    let b' =
      let ty = Type.bool in
      Annot.bool LocTest.dummy true
        |> Annot.binding LocTest.dummy "id-two" ty
    in
    Annot.top_bind_rec LocTest.dummy [b; b']
  in
  let expected' =
    let ty = Type.bool in
    Annot.bool LocTest.dummy true
      |> Annot.binding LocTest.dummy "id-two" ty
      |> Annot.top_bind LocTest.dummy
  in
  let file = Annot.file [expected; expected'] in
  match file with
    | actual :: actual' :: [] ->
      assert_top_equal ~ctxt expected actual;
      assert_top_equal ~ctxt expected' actual'
    | actual -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Annot.pp_file) ~msg:"Source files are not equal" file actual

let test_constructor =
  "Constructor" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Booleans"  >:: test_bool;
        "Integers"  >:: test_int;
        "Variables" >:: test_var;
      ];
      "Operators" >::: [
        "Unary" >::: [
          "Boolean Not" >:: test_not;
        ];
        "Binary" >::: [
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
      ];
      "Conditional"              >:: test_cond;
      "Value Binding"            >:: test_bind;
      "Recursive Value Bindings" >:: test_bind_rec;
      "Function Abstraction"     >:: test_abs;
      "Function Application"     >:: test_app;
    ];
    "Top-Level Statements" >::: [
      "Value Binding"            >:: test_top_bind;
      "Recursive Value Bindings" >:: test_top_bind_rec;
    ];
    "Files" >:: test_file;
  ]

(* Annotation *)

let test_annotate =
  "Annotation" >::: [
  ]

(* Location Tracking *)

let assert_loc_expr ~ctxt loc ast =
  ast
    |> Annot.loc_expr
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc_bool ctxt =
  let loc = LocTest.gen () in
  Annot.bool loc true
    |> assert_loc_expr ~ctxt loc

let test_loc_int ctxt =
  let loc = LocTest.gen () in
  Annot.int loc 1
    |> assert_loc_expr ~ctxt loc

let test_loc_var ctxt =
  let loc = LocTest.gen () in
  Annot.var loc "x"
    |> assert_loc_expr ~ctxt loc

let test_loc_un_op ctxt =
  let loc = LocTest.gen () in
  Annot.bool LocTest.dummy true
    |> Annot.un_op loc Op.un_not
    |> assert_loc_expr ~ctxt loc

let test_loc_bin_op ctxt =
  let loc = LocTest.gen () in
  let l = Annot.int LocTest.dummy 1 in
  let r = Annot.int LocTest.dummy 2 in
  Annot.bin_op loc l Op.bin_add r
    |> assert_loc_expr ~ctxt loc

let test_loc_cond ctxt =
  let loc = LocTest.gen () in
  let c = Annot.bool LocTest.dummy true in
  let t = Annot.int LocTest.dummy 1 in
  let f = Annot.int LocTest.dummy 2 in
  Annot.cond loc c t f
    |> assert_loc_expr ~ctxt loc

let test_loc_bind ctxt =
  let loc = LocTest.gen () in
  let b =
    Annot.int LocTest.dummy 1
      |> Annot.binding LocTest.dummy "x" Type.int
  in
  Annot.var LocTest.dummy "x"
    |> Annot.bind loc b
    |> assert_loc_expr ~ctxt loc

let test_loc_bind_rec ctxt =
  let loc = LocTest.gen () in
  let bs = [
    Annot.int LocTest.dummy 1
      |> Annot.binding LocTest.dummy "x" Type.int;
    Annot.int LocTest.dummy 2
      |> Annot.binding LocTest.dummy "y" Type.int
  ] in
  let x = Annot.var LocTest.dummy "x" in
  let y = Annot.var LocTest.dummy "y" in
  Annot.bin_op LocTest.dummy x Op.bin_eq y
    |> Annot.bind_rec loc bs
    |> assert_loc_expr ~ctxt loc

let test_loc_abs ctxt =
  let loc = LocTest.gen () in
  Annot.int LocTest.dummy 1
    |> Annot.abs loc "param" Type.int Type.int
    |> assert_loc_expr ~ctxt loc

let test_loc_app ctxt =
  let loc = LocTest.gen () in
  let f = Annot.var LocTest.dummy "f" in
  Annot.var LocTest.dummy "x"
    |> Annot.app loc f
    |> assert_loc_expr ~ctxt loc

let test_loc_top_bind ctxt =
  let loc = LocTest.gen () in
  Annot.top_bind loc b
    |> Annot.loc_top
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc_top_bind_rec ctxt =
  let loc = LocTest.gen () in
  Annot.top_bind_rec loc [b; b']
    |> Annot.loc_top
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc =
  "Location Tracking" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Integer"  >:: test_loc_int;
        "Boolean"  >:: test_loc_bool;
        "Variable" >:: test_loc_var;
      ];
      "Operators" >::: [
        "Unary"  >:: test_loc_un_op;
        "Binary" >:: test_loc_bin_op;
      ];
      "Conditional"             >:: test_loc_cond;
      "Value Binding"           >:: test_loc_bind;
      "Recursive Value Binding" >:: test_loc_bind_rec;
      "Function Abstraction"    >:: test_loc_abs;
      "Function Application"    >:: test_loc_app;
    ];
    "Top-Level Statements" >::: [
      "Value Binding"            >:: test_loc_top_bind;
      "Recursive Value Bindings" >:: test_loc_top_bind_rec;
    ]
  ]

(* Pretty Printing *)

let assert_pp_expr = CommonTest.Util.assert_pp Annot.pp_expr

let id_f = "f"
let id_w = "w"
let id_x = "x"
let id_y = "y"
let id_z = "z"
let id_temp_one = "temporaryVariableOne"
let id_temp_two = "temporaryVariableTwo"

let var_f = Annot.var LocTest.dummy id_f
let var_w = Annot.var LocTest.dummy id_w
let var_x = Annot.var LocTest.dummy id_x
let var_y = Annot.var LocTest.dummy id_y
let var_z = Annot.var LocTest.dummy id_z
let var_temp_one = Annot.var LocTest.dummy id_temp_one
let var_temp_two = Annot.var LocTest.dummy id_temp_two

let expr_temp_one = Annot.bin_op LocTest.dummy var_w Op.bin_add var_x
let expr_temp_two = Annot.bin_op LocTest.dummy var_y Op.bin_add var_z
let expr_result = Annot.bin_op LocTest.dummy var_temp_one Op.bin_add var_temp_two

let b_temp_one = Annot.binding LocTest.dummy id_temp_one Type.int expr_temp_one
let b_temp_two = Annot.binding LocTest.dummy id_temp_two Type.int expr_temp_two

let temp_two = Annot.bind LocTest.dummy b_temp_two expr_result
let temp_one = Annot.bind LocTest.dummy b_temp_one temp_two

let test_pp_bool ctxt =
  Annot.bool LocTest.dummy true
    |> assert_pp_expr ~ctxt  ["true"];
  Annot.bool LocTest.dummy false
    |> assert_pp_expr ~ctxt ["false"]

let test_pp_int ctxt =
  Annot.int LocTest.dummy 1
    |> assert_pp_expr ~ctxt ["1"];
  Annot.int LocTest.dummy 42
    |> assert_pp_expr ~ctxt ["42"];
  Annot.int LocTest.dummy (-10)
    |> assert_pp_expr ~ctxt ["-10"]

let test_pp_var ctxt =
  Annot.var LocTest.dummy "variableName"
    |> assert_pp_expr ~ctxt ["variableName"]

let test_pp_un_op_constant ctxt =
  Annot.bool LocTest.dummy true
    |> Annot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!true"]

let test_pp_un_op_equal_precedence ctxt =
  Annot.bool LocTest.dummy true
    |> Annot.un_op LocTest.dummy Op.un_not
    |> Annot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!!true"]

let test_pp_un_op_higher_precedence ctxt =
  Annot.bool LocTest.dummy true
    |> Annot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!true"]

let test_pp_un_op_lower_precedence ctxt =
  Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 1) Op.bin_eq (Annot.int LocTest.dummy 2)
    |> Annot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!(1 == 2)"]

let test_pp_bin_op_constant ctxt =
  Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 1) Op.bin_add (Annot.int LocTest.dummy 2)
    |> assert_pp_expr ~ctxt ["1 + 2"]

let test_pp_bin_op_equal_precedence ctxt =
  let lhs = Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 1) Op.bin_add (Annot.int LocTest.dummy 2) in
  let rhs = Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 3) Op.bin_add (Annot.int LocTest.dummy 4) in
  Annot.bin_op LocTest.dummy lhs Op.bin_sub rhs
    |> assert_pp_expr ~ctxt ["1 + 2 - 3 + 4"];
  rhs
    |> Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 2) Op.bin_sub
    |> Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 1) Op.bin_add
    |> assert_pp_expr ~ctxt ["1 + 2 - 3 + 4"]

let test_pp_bin_op_higher_precedence ctxt =
  let lhs = Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 1) Op.bin_mul (Annot.int LocTest.dummy 2) in
  let rhs = Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 3) Op.bin_mul (Annot.int LocTest.dummy 4) in
  let const = Annot.int LocTest.dummy 5 in

  Annot.bin_op LocTest.dummy lhs Op.bin_add const
    |> assert_pp_expr ~ctxt ["1 * 2 + 5"];
  Annot.bin_op LocTest.dummy const Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["5 + 3 * 4"];
  Annot.bin_op LocTest.dummy lhs Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["1 * 2 + 3 * 4"]

let test_pp_bin_op_lower_precedence ctxt =
  let lhs = Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 1) Op.bin_eq (Annot.int LocTest.dummy 2) in
  let rhs = Annot.bin_op LocTest.dummy (Annot.int LocTest.dummy 3) Op.bin_eq (Annot.int LocTest.dummy 4) in
  let const = Annot.int LocTest.dummy 5 in

  Annot.bin_op LocTest.dummy lhs Op.bin_add const
    |> assert_pp_expr ~ctxt ["(1 == 2) + 5"];
  Annot.bin_op LocTest.dummy const Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["5 + (3 == 4)"];
  Annot.bin_op LocTest.dummy lhs Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["(1 == 2) + (3 == 4)"]

let test_pp_bin_op_long ctxt =
  let name = String.make 50 'a' in
  let var = Annot.var LocTest.dummy name in
  Annot.bin_op LocTest.dummy var Op.bin_add var
    |> assert_pp_expr ~ctxt [
         name ^ " +";
         "  " ^ name;
       ]

let test_pp_cond_one_line ctxt =
  let c = Annot.var LocTest.dummy "c" in
  let t = Annot.var LocTest.dummy "t" in
  let f = Annot.var LocTest.dummy "f" in

  Annot.cond LocTest.dummy c t f
    |> assert_pp_expr ~ctxt ["if c then t else f"]

let test_pp_cond_three_line ctxt =
  let c = Annot.var LocTest.dummy "conditionIsAVeryLongExpression" in
  let t = Annot.var LocTest.dummy "trueBranchIsAVeryLongExpression" in
  let f = Annot.var LocTest.dummy "falseBranchIsAVeryLongExpression" in

  Annot.cond LocTest.dummy c t f
    |> assert_pp_expr ~ctxt [
         "if conditionIsAVeryLongExpression";
         "then trueBranchIsAVeryLongExpression";
         "else falseBranchIsAVeryLongExpression";
       ]

let test_pp_cond_full_spread ctxt =
  let c = Annot.var LocTest.dummy "conditionIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in
  let t = Annot.var LocTest.dummy "trueBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in
  let f = Annot.var LocTest.dummy "falseBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in

  Annot.cond LocTest.dummy c t f
    |> assert_pp_expr ~ctxt [
         "if";
         "  conditionIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
         "then";
         "  trueBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
         "else";
         "  falseBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
       ]

let test_pp_bind_simple_var ctxt =
  let b = Annot.binding LocTest.dummy id_x Type.int (Annot.int LocTest.dummy 1) in
  Annot.bind LocTest.dummy b var_x
    |> assert_pp_expr ~ctxt [
         "let x: Int = 1 in";
         "x";
       ]

let test_pp_bind_compound_var ctxt =
  let id_final = "finalVariable" in
  let v_final = Annot.var LocTest.dummy id_final in
  let b_final = Annot.binding LocTest.dummy id_final Type.int temp_one in

  Annot.bind LocTest.dummy b_final v_final
    |> assert_pp_expr ~ctxt [
         "let finalVariable: Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "finalVariable";
       ]

let test_pp_bind_simple_function ctxt =
  let fn =
    Annot.abs LocTest.dummy id_x Type.int Type.int expr_temp_one
      |> Annot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int)
  in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Annot.binding LocTest.dummy id_f ty fn in

  Annot.bind LocTest.dummy b var_f
    |> assert_pp_expr ~ctxt [
         "let f(w: Int, x: Int): Int = w + x in";
         "f";
       ]

let test_pp_bind_compound_function ctxt =
  let body = Annot.bind LocTest.dummy b_temp_one temp_two in
  let fn =
    Annot.abs LocTest.dummy id_z Type.int Type.int body
      |> Annot.abs LocTest.dummy id_y Type.int (Type.func Type.int Type.int)
      |> Annot.abs LocTest.dummy id_x Type.int (Type.func Type.int (Type.func Type.int Type.int))
      |> Annot.abs LocTest.dummy id_w Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int)))
  in
  let ty = Type.func Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int))) in
  let b = Annot.binding LocTest.dummy id_f ty fn in

  Annot.bind LocTest.dummy b var_f
    |> assert_pp_expr ~ctxt [
         "let f(w: Int, x: Int, y: Int, z: Int): Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "f";
       ]

let test_pp_bind_complex_result ctxt =
  let fn = Annot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int) expr_temp_one in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Annot.binding LocTest.dummy id_f ty fn in

  Annot.bind LocTest.dummy b var_f
    |> assert_pp_expr ~ctxt [
         "let f(w: Int): Int -> Int = w + x in";
         "f";
       ]

let test_pp_bind_rec_simple_var ctxt =
  let b_x = Annot.binding LocTest.dummy id_x Type.int var_y in
  let b_y = Annot.binding LocTest.dummy id_y Type.int var_x in
  let res = Annot.bin_op LocTest.dummy var_x Op.bin_add var_y in

  Annot.bind_rec LocTest.dummy [b_x] res
    |> assert_pp_expr ~ctxt [
         "let rec x: Int = y in";
         "x + y";
       ];
  Annot.bind_rec LocTest.dummy [b_x; b_y] res
    |> assert_pp_expr ~ctxt [
         "let rec x: Int = y and y: Int = x in";
         "x + y";
       ]

let test_pp_bind_rec_compound_var ctxt =
  let id_final = "finalVariable" in
  let v_final = Annot.var LocTest.dummy id_final in
  let b_final = Annot.binding LocTest.dummy id_final Type.int temp_one in

  Annot.bind_rec LocTest.dummy [b_final] v_final
    |> assert_pp_expr ~ctxt [
         "let rec finalVariable: Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "finalVariable";
       ];
  Annot.bind_rec LocTest.dummy [b_final; b_final] v_final
    |> assert_pp_expr ~ctxt [
         "let rec finalVariable: Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "and finalVariable: Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "finalVariable";
       ]

let test_pp_bind_rec_simple_function ctxt =
  let fn =
    Annot.abs LocTest.dummy id_x Type.int Type.int expr_temp_one
      |> Annot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int)
  in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Annot.binding LocTest.dummy id_f ty fn in

  Annot.bind_rec LocTest.dummy [b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int): Int = w + x in";
         "f";
       ];
  Annot.bind_rec LocTest.dummy [b; b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int): Int = w + x and f(w: Int, x: Int): Int = w + x in";
         "f";
       ]

let test_pp_bind_rec_compound_function ctxt =
  let body = Annot.bind LocTest.dummy b_temp_one temp_two in
  let fn =
    Annot.abs LocTest.dummy id_z Type.int Type.int body
      |> Annot.abs LocTest.dummy id_y Type.int (Type.func Type.int Type.int)
      |> Annot.abs LocTest.dummy id_x Type.int (Type.func Type.int (Type.func Type.int Type.int))
      |> Annot.abs LocTest.dummy id_w Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int)))
  in
  let ty = Type.func Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int))) in
  let b = Annot.binding LocTest.dummy id_f ty fn in

  Annot.bind_rec LocTest.dummy [b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int, y: Int, z: Int): Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "f";
       ];
  Annot.bind_rec LocTest.dummy [b; b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int, y: Int, z: Int): Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "and f(w: Int, x: Int, y: Int, z: Int): Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "f";
       ]

let test_pp_bind_rec_complex_result ctxt =
  let fn = Annot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int) expr_temp_one in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Annot.binding LocTest.dummy id_f ty fn in

  Annot.bind_rec LocTest.dummy [b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int): Int -> Int = w + x in";
         "f";
       ];
  Annot.bind_rec LocTest.dummy [b; b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int): Int -> Int = w + x and f(w: Int): Int -> Int = w + x in";
         "f";
       ]

let test_pp_app_named ctxt =
  let x = Annot.int LocTest.dummy 1 in
  let y = Annot.bool LocTest.dummy true in

  let app_one = Annot.app LocTest.dummy var_f x in
  Annot.app LocTest.dummy app_one y
    |> assert_pp_expr ~ctxt ["f 1 true"]

let test_pp_app_literal ctxt =
  let f =
    Annot.bin_op LocTest.dummy var_x Op.bin_eq var_y
      |> Annot.abs LocTest.dummy id_y Type.int Type.bool
      |> Annot.abs LocTest.dummy id_x Type.int (Type.func Type.int Type.bool)
  in
  let x = Annot.int LocTest.dummy 1 in
  let y = Annot.bool LocTest.dummy true in

  let app_one = Annot.app LocTest.dummy f x in
  Annot.app LocTest.dummy app_one y
    |> assert_pp_expr ~ctxt ["((x: Int, y: Int): Bool => x == y) 1 true"]

let test_pp_app_func_result ctxt =
  let f = Annot.app LocTest.dummy var_f var_x in
  let x = Annot.int LocTest.dummy 1 in
  let y = Annot.bool LocTest.dummy true in

  let app_one = Annot.app LocTest.dummy f x in
  Annot.app LocTest.dummy app_one y
    |> assert_pp_expr ~ctxt ["(f x) 1 true"]

let test_pp_app_parenthesized ctxt =
  let x =
    let x = Annot.int LocTest.dummy 1 in
    let y = Annot.int LocTest.dummy 2 in
    Annot.bin_op LocTest.dummy x Op.bin_add y
  in
  let y =
    let x = Annot.bool LocTest.dummy true in
    let y = Annot.bool LocTest.dummy false in
    Annot.bin_op LocTest.dummy x Op.bin_and y
  in
  let z =
    let g = Annot.var LocTest.dummy "g" in
    let z = Annot.var LocTest.dummy "z" in
    Annot.app LocTest.dummy g z
  in

  let app_one = Annot.app LocTest.dummy var_f x in
  let app_two = Annot.app LocTest.dummy app_one y in
  Annot.app LocTest.dummy app_two z
    |> assert_pp_expr ~ctxt ["f (1 + 2) (true && false) (g z)"]

let test_pp_app_wrap ctxt =
  let f = Annot.var LocTest.dummy "aReallyLongFunctionName" in
  let x = Annot.var LocTest.dummy "aLongParameterName" in
  let y = Annot.var LocTest.dummy "yetAnotherReallyLongParameterName" in
  let z = Annot.var LocTest.dummy "whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames" in

  let app_one = Annot.app LocTest.dummy f x in
  let app_two = Annot.app LocTest.dummy app_one y in
  Annot.app LocTest.dummy app_two z
    |> assert_pp_expr ~ctxt [
         "aReallyLongFunctionName aLongParameterName yetAnotherReallyLongParameterName";
         "  whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames";
       ]

let assert_pp_top = CommonTest.Util.assert_pp Annot.pp_top

let test_pp_top_bind ctxt =
  Annot.top_bind LocTest.dummy b
    |> assert_pp_top ~ctxt [
         "let id-one: Int = 1";
       ]

let test_pp_top_bind_rec ctxt =
  Annot.top_bind_rec LocTest.dummy [b; b']
    |> assert_pp_top ~ctxt [
         "let rec id-one: Int = 1";
         "and id-two: Bool = true"
       ]

let assert_pp_file = CommonTest.Util.assert_pp Annot.pp_file

let test_pp_file ctxt =
  let expected =
    let b =
      let ty = Type.int in
      Annot.int LocTest.dummy 1
        |> Annot.binding LocTest.dummy "id-one" ty
    in
    let b' =
      let ty = Type.bool in
      Annot.bool LocTest.dummy true
        |> Annot.binding LocTest.dummy "id-two" ty
    in
    Annot.top_bind_rec LocTest.dummy [b; b']
  in
  let expected' =
    let ty = Type.bool in
    Annot.bool LocTest.dummy true
      |> Annot.binding LocTest.dummy "id-two" ty
      |> Annot.top_bind LocTest.dummy
  in

  Annot.file [expected; expected']
    |> assert_pp_file ~ctxt [
         "let rec id-one: Int = 1";
         "and id-two: Bool = true";
         "";
         "let id-two: Bool = true";
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Boolean"  >:: test_pp_bool;
        "Integer"  >:: test_pp_int;
        "Variable" >:: test_pp_var;
      ];
      "Operators" >::: [
        "Unary" >::: [
          "Constant"          >:: test_pp_un_op_constant;
          "Equal Precedence"  >:: test_pp_un_op_equal_precedence;
          "Higher Precedence" >:: test_pp_un_op_higher_precedence;
          "Lower Precedence"  >:: test_pp_un_op_lower_precedence;
        ];
        "Binary" >::: [
          "Constant"          >:: test_pp_bin_op_constant;
          "Equal Precedence"  >:: test_pp_bin_op_equal_precedence;
          "Higher Precedence" >:: test_pp_bin_op_higher_precedence;
          "Lower Precedence"  >:: test_pp_bin_op_lower_precedence;
          "Long"              >:: test_pp_bin_op_long;
        ]
      ];
      "Conditional" >::: [
        "One Line"    >:: test_pp_cond_one_line;
        "Three Line"  >:: test_pp_cond_three_line;
        "Full Spread" >:: test_pp_cond_full_spread;
      ];
      "Value Binding" >::: [
        "Simple Variables"     >:: test_pp_bind_simple_var;
        "Compound Variables"   >:: test_pp_bind_compound_var;
        "Simple Function"      >:: test_pp_bind_simple_function;
        "Compound Function"    >:: test_pp_bind_compound_function;
        "Complex Result Value" >:: test_pp_bind_complex_result;
      ];
      "Recursive Value Bindings" >::: [
        "Simple Variables"      >:: test_pp_bind_rec_simple_var;
        "Compound Variables"    >:: test_pp_bind_rec_compound_var;
        "Simple Functions"      >:: test_pp_bind_rec_simple_function;
        "Compound Functions"    >:: test_pp_bind_rec_compound_function;
        "Complex Result Values" >:: test_pp_bind_rec_complex_result;
      ];
      "Function Abstraction" >::: [

      ];
      "Function Application" >::: [
        "Named Function"          >:: test_pp_app_named;
        "Literal Function"        >:: test_pp_app_literal;
        "Functional Result"       >:: test_pp_app_func_result;
        "Parenthesized Arguments" >:: test_pp_app_parenthesized;
        "Argument Wraping"        >:: test_pp_app_wrap;
      ]
    ];
    "Top-Level Statements" >::: [
      "Value Binding"           >:: test_pp_top_bind;
      "Recursive Value Binding" >:: test_pp_top_bind_rec;
    ];
    "File" >:: test_pp_file;
  ]

(* Type Checking *)

let assert_type_of_expr ~ctxt ?env:(env = Annot.env) ty ast =
  ast
    |> Annot.type_of_expr env
    |> CommonTest.TypeTest.assert_type_equal ~ctxt ty

let assert_should_raise ~env ast =
  ast
    |> Annot.type_of_expr env
    |> Type.pp
    |> fprintf str_formatter "Expected a type error, but got type \"%t\""
    |> flush_str_formatter
    |> assert_failure

let assert_invalid_unary_operand ~ctxt ?env:(env = Annot.env) expected op r ast =
  try assert_should_raise ~env ast
  with
    | Check.InvalidUnaryOperand(expected', op', r') ->
      assert_equal ~ctxt expected expected';
      assert_equal ~ctxt op op';
      assert_equal ~ctxt r r'
    | exn -> raise exn

let assert_invalid_binary_operand ~ctxt ?env:(env = Annot.env) expected l op r ast =
  try assert_should_raise ~env ast
  with
    | Check.InvalidBinaryOperands(expected', l', op', r') ->
      assert_equal ~ctxt expected expected';
      assert_equal ~ctxt l l';
      assert_equal ~ctxt op op';
      assert_equal ~ctxt r r'
    | exn -> raise exn

let assert_invalid_equality_operand ~ctxt ?env:(env = Annot.env) l op r ast =
  try assert_should_raise ~env ast
  with
    | Check.InvalidEqualityOperands(l', op', r') ->
      assert_equal ~ctxt l l';
      assert_equal ~ctxt op op';
      assert_equal ~ctxt r r'
    | exn -> raise exn

let assert_declaration_mismatch ~ctxt ?env:(env = Annot.env) id expected actual ast =
  try assert_should_raise ~env ast
  with
    | Check.DeclarationMismatch (id', expected', actual') ->
      assert_equal ~ctxt id id';
      CommonTest.TypeTest.assert_type_equal ~ctxt expected expected';
      CommonTest.TypeTest.assert_type_equal ~ctxt actual actual'
    | exn -> raise exn

let assert_result_mismatch ~ctxt ?env:(env = Annot.env) expected actual ast =
  try assert_should_raise ~env ast
  with
    | Check.ResultMismatch (expected', actual') ->
      CommonTest.TypeTest.assert_type_equal ~ctxt expected expected';
      CommonTest.TypeTest.assert_type_equal ~ctxt actual actual'
    | exn -> raise exn

let assert_unbound_identifier ~ctxt ?env:(env = Annot.env) id ast =
  try assert_should_raise ~env ast
  with
    | Check.UnboundIdentifier id' ->
      assert_equal ~ctxt id id'
    | exn -> raise exn

let assert_cannot_apply ~ctxt ?env:(env = Annot.env) ty ast =
  try assert_should_raise ~env ast
  with
    | Check.CannotApply ty' ->
      CommonTest.TypeTest.assert_type_equal ~ctxt ty ty'
    | exn -> raise exn

let assert_too_many_args ~ctxt ?env:(env = Annot.env) ty num ast =
  try assert_should_raise ~env ast
  with
    | Check.TooManyArgs (ty', num') ->
      CommonTest.TypeTest.assert_type_equal ~ctxt ty ty';
      assert_equal ~ctxt num num'
    | exn -> raise exn

let assert_invalid_args ~ctxt ?env:(env = Annot.env) expected actual ast =
  try assert_should_raise ~env ast
  with
    | Check.InvalidArgs (expected', actual') ->
      CommonTest.TypeTest.assert_type_equal ~ctxt expected expected';
      CommonTest.TypeTest.assert_type_equal ~ctxt actual actual'
    | exn -> raise exn

let assert_invalid_condition ~ctxt ?env:(env = Annot.env) expected ast =
  try assert_should_raise ~env ast
  with
    | Check.InvalidCondition actual ->
      CommonTest.TypeTest.assert_type_equal ~ctxt expected actual
    | exn -> raise exn

let assert_conditional_branch_mismatch ~ctxt ?env:(env = Annot.env) t f ast =
  try assert_should_raise ~env ast
  with
    | Check.ConditionalBranchMismatch (t', f') ->
      CommonTest.TypeTest.assert_type_equal ~ctxt t t';
      CommonTest.TypeTest.assert_type_equal ~ctxt f f'
    | exn -> raise exn

let i = Ast.int LocTest.dummy 1
let b = Ast.bool LocTest.dummy true

let id = "x"
let x = Ast.var LocTest.dummy id

let test_type_of_bool ctxt =
  b
    |> assert_type_of_expr ~ctxt Type.bool

let test_type_of_int ctxt =
  i
    |> assert_type_of_expr ~ctxt Type.int

let test_type_of_var_bound ctxt =
  let ty = Type.int in
  let env = Check.bind id ty Check.env in

  assert_type_of_expr ~ctxt ~env ty x

let test_type_of_var_unbound ctxt =
  assert_unbound_identifier ~ctxt id x

let test_type_of_un_op_not_lit ctxt =
  b
    |> Ast.un_op LocTest.dummy Op.un_not
    |> assert_type_of_expr ~ctxt Type.bool

let test_type_of_un_op_not_var ctxt =
  let env = Check.bind id Type.bool Check.env in
  x
    |> Ast.un_op LocTest.dummy Op.un_not
    |> assert_type_of_expr ~ctxt ~env Type.bool

let test_type_of_un_op_not_invalid_argument ctxt =
  i
    |> Ast.un_op LocTest.dummy Op.un_not
    |> assert_invalid_unary_operand ~ctxt Type.bool Op.un_not Type.int

let type_of_int_test op expected name =
  let test_lit ctxt =
    Ast.bin_op LocTest.dummy i op i
      |> assert_type_of_expr ~ctxt expected
  in
  let test_invalid_argument ctxt =
    Ast.bin_op LocTest.dummy b op i
      |> assert_invalid_binary_operand ~ctxt Type.int Type.bool op Type.int;
    Ast.bin_op LocTest.dummy i op b
      |> assert_invalid_binary_operand ~ctxt Type.int Type.int op Type.bool;
    Ast.bin_op LocTest.dummy b op b
      |> assert_invalid_binary_operand ~ctxt Type.int Type.bool op Type.bool
  in
  name >::: [
    "Literal"                >:: test_lit;
    "Invalid Argument Types" >:: test_invalid_argument;
  ]

let type_of_bool_test op expected name =
  let test_lit ctxt =
    Ast.bin_op LocTest.dummy b op b
      |> assert_type_of_expr ~ctxt expected
  in
  let test_invalid_argument ctxt =
    Ast.bin_op LocTest.dummy i op b
      |> assert_invalid_binary_operand ~ctxt Type.bool Type.int op Type.bool;
    Ast.bin_op LocTest.dummy b op i
      |> assert_invalid_binary_operand ~ctxt Type.bool Type.bool op Type.int;
    Ast.bin_op LocTest.dummy i op i
      |> assert_invalid_binary_operand ~ctxt Type.bool Type.int op Type.int
  in
  name >::: [
    "Literal"                >:: test_lit;
    "Invalid Argument Types" >:: test_invalid_argument;
  ]

let type_of_eq_test op expected name =
  let test_lit ctxt =
    Ast.bin_op LocTest.dummy b op b
      |> assert_type_of_expr ~ctxt expected;
    Ast.bin_op LocTest.dummy i op i
      |> assert_type_of_expr ~ctxt expected
  in
  let test_invalid_argument ctxt =
    Ast.bin_op LocTest.dummy i op b
      |> assert_invalid_equality_operand ~ctxt Type.int op Type.bool;
    Ast.bin_op LocTest.dummy b op i
      |> assert_invalid_equality_operand ~ctxt Type.bool op Type.int
  in
  name >::: [
    "Literal"                >:: test_lit;
    "Invalid Argument Types" >:: test_invalid_argument;
  ]

let c = Ast.bool LocTest.dummy true
let t = Ast.int LocTest.dummy 1
let f = Ast.int LocTest.dummy 2

let test_type_of_cond ctxt =
  Ast.cond LocTest.dummy c t f
    |> assert_type_of_expr ~ctxt Type.int

let test_type_of_cond_invalid_condition ctxt =
  Ast.cond LocTest.dummy t t f
    |> assert_invalid_condition ~ctxt Type.int

let test_type_of_cond_branch_mismatch ctxt =
  Ast.cond LocTest.dummy c t c
    |> assert_conditional_branch_mismatch ~ctxt Type.int Type.bool

let id = "x"
let ty = Type.int
let v = Ast.int LocTest.dummy 42
let b = Ast.binding LocTest.dummy id ty v
let rest = Ast.var LocTest.dummy id

let test_type_of_bind ctxt =
  Ast.bind LocTest.dummy b rest
    |> assert_type_of_expr ~ctxt ty

let test_type_of_bind_mismatched_types ctxt =
  let v = Ast.bool LocTest.dummy true in
  let b = Ast.binding LocTest.dummy id ty v in
  let rest = Ast.var LocTest.dummy "unbound-identifier" in

  Ast.bind LocTest.dummy b rest
    |> assert_declaration_mismatch ~ctxt id ty Type.bool

let idx = "x"
let idy = "y"

let vx = Ast.var LocTest.dummy idx
let vy = Ast.var LocTest.dummy idy

let tx = Type.int
let ty = Type.int

let bx = Ast.bin_op LocTest.dummy vy Op.bin_add (Ast.int LocTest.dummy 1)
let by = Ast.bin_op LocTest.dummy vx Op.bin_add (Ast.int LocTest.dummy 2)

let bs = [
  Ast.binding LocTest.dummy idx tx bx;
  Ast.binding LocTest.dummy idy ty by;
]

let ty = Type.bool
let rest = Ast.bin_op LocTest.dummy vx Op.bin_eq vy

let test_type_of_bind_rec ctxt =
  Ast.bind_rec LocTest.dummy bs rest
    |> assert_type_of_expr ~ctxt ty

let test_type_of_bind_rec_incorrect_types ctxt =
  let bs = [
    Ast.binding LocTest.dummy idx Type.bool bx;
    Ast.binding LocTest.dummy idy ty by;
  ] in
  let unbound = Ast.var LocTest.dummy "unbound-identifier" in
  let unbound' = Ast.var LocTest.dummy "another-unbound-identifier" in
  let rest = Ast.bin_op LocTest.dummy unbound Op.bin_eq unbound' in

  Ast.bind_rec LocTest.dummy bs rest
    |> assert_invalid_binary_operand ~ctxt Type.int Type.bool Op.bin_add Type.int

let test_type_of_bind_rec_declaration_mismatch ctxt =
  let bs = [
    Ast.binding LocTest.dummy idx Type.bool bx;
    Ast.binding LocTest.dummy idy Type.int (Ast.int LocTest.dummy 1);
  ] in
  let unbound = Ast.var LocTest.dummy "unbound-identifier" in
  let unbound' = Ast.var LocTest.dummy "another-unbound-identifier" in
  let rest = Ast.bin_op LocTest.dummy unbound Op.bin_eq unbound' in

  Ast.bind_rec LocTest.dummy bs rest
    |> assert_declaration_mismatch ~ctxt idx Type.bool tx

let idx = "x"
let tx = Type.int

let idy = "y"
let ty = Type.int

let vx = Ast.var LocTest.dummy idx
let vy = Ast.var LocTest.dummy idy
let b = Ast.bin_op LocTest.dummy vx Op.bin_eq vy
let tb = Type.bool

let test_type_of_abs ctxt =
  let expected = Type.func tx (Type.func ty tb) in

  Ast.abs LocTest.dummy idy ty tb b
    |> Ast.abs LocTest.dummy idx tx (Type.func ty tb)
    |> assert_type_of_expr ~ctxt expected

let test_type_of_abs_result_mismatch ctxt =
  Ast.abs LocTest.dummy idy ty Type.int b
    |> Ast.abs LocTest.dummy idx tx (Type.func Type.int Type.int)
    |> assert_result_mismatch ~ctxt Type.int tb

let id = "fn"
let fn = Ast.var LocTest.dummy id
let res = Type.int
let ty' = Type.func Type.bool res
let ty = Type.func Type.int ty'
let env = Check.bind id ty Check.env

let x = Ast.int LocTest.dummy 42
let y = Ast.bool LocTest.dummy true
let z = Ast.int LocTest.dummy 0

let test_type_of_app ctxt =
  let app_one = Ast.app LocTest.dummy fn x in
  Ast.app LocTest.dummy app_one y
    |> assert_type_of_expr ~ctxt ~env Type.int

let test_type_of_app_partial ctxt =
  Ast.app LocTest.dummy fn x
    |> assert_type_of_expr ~ctxt ~env ty'

let test_type_of_app_cannot_apply ctxt =
  let ty = Type.int in
  let env = Check.bind id ty Check.env in
  let app_one = Ast.app LocTest.dummy fn x in
  Ast.app LocTest.dummy app_one y
    |> assert_cannot_apply ~ctxt ~env ty

let test_type_of_app_too_many_args ctxt =
  let app_one = Ast.app LocTest.dummy fn x in
  let app_two = Ast.app LocTest.dummy app_one y in
  Ast.app LocTest.dummy app_two z
    |> assert_too_many_args ~ctxt ~env ty 3

let test_type_of_app_invalid_args ctxt =
  let app_one = Ast.app LocTest.dummy fn y in
  Ast.app LocTest.dummy app_one x
    |> assert_invalid_args ~ctxt ~env Type.int Type.bool;
  let app_one = Ast.app LocTest.dummy fn x in
  Ast.app LocTest.dummy app_one z
    |> assert_invalid_args ~ctxt ~env Type.bool Type.int

let test_type_of =
  "Type Checking" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Boolean"  >:: test_type_of_bool;
        "Integer"  >:: test_type_of_int;
        "Variable" >::: [
          "Bound"   >:: test_type_of_var_bound;
          "Unbound" >:: test_type_of_var_unbound;
        ]
      ];
      "Operators" >::: [
        "Unary" >::: [
          "Boolean Negation" >::: [
            "Literal"               >:: test_type_of_un_op_not_lit;
            "Variable"              >:: test_type_of_un_op_not_var;
            "Invalid Argument Type" >:: test_type_of_un_op_not_invalid_argument;
          ]
        ];
        "Binary" >::: [
          "Addition"              |> type_of_int_test  Op.bin_add Type.int;
          "Subtraction"           |> type_of_int_test  Op.bin_sub Type.int;
          "Multiplication"        |> type_of_int_test  Op.bin_mul Type.int;
          "Integer Division"      |> type_of_int_test  Op.bin_div Type.int;
          "Modulus"               |> type_of_int_test  Op.bin_mod Type.int;
          "Logical And"           |> type_of_bool_test Op.bin_and Type.bool;
          "Logical Or"            |> type_of_bool_test Op.bin_or  Type.bool;
          "Equality"              |> type_of_eq_test   Op.bin_eq  Type.bool;
          "Inequality"            |> type_of_eq_test   Op.bin_neq Type.bool;
          "Less Than or Equal"    |> type_of_int_test  Op.bin_lte Type.bool;
          "Less Than"             |> type_of_int_test  Op.bin_lt  Type.bool;
          "Greater Than"          |> type_of_int_test  Op.bin_gt  Type.bool;
          "Greater Than or Equal" |> type_of_int_test  Op.bin_gte Type.bool;
        ]
      ];
      "Conditional" >::: [
        "Success"           >:: test_type_of_cond;
        "Invalid Condition" >:: test_type_of_cond_invalid_condition;
        "Branch Mismatch"   >:: test_type_of_cond_branch_mismatch;
      ];
      "Value Binding" >::: [
        "Success"          >:: test_type_of_bind;
        "Mismatched Types" >:: test_type_of_bind_mismatched_types;
      ];
      "Recursive Value Bindings" >::: [
        "Success"              >:: test_type_of_bind_rec;
        "Incorrect Types"      >:: test_type_of_bind_rec_incorrect_types;
        "Declaration Mismatch" >:: test_type_of_bind_rec_declaration_mismatch
      ];
      "Function Abstraction" >::: [
        "Success"         >:: test_type_of_abs;
        "Result Mismatch" >:: test_type_of_abs_result_mismatch;
      ];
      "Function Application" >::: [
        "Success"                >:: test_type_of_app;
        "Partial Application"    >:: test_type_of_app_partial;
        "Cannot Apply"           >:: test_type_of_app_cannot_apply;
        "Too Many Arguments"     >:: test_type_of_app_too_many_args;
        "Invalid Argument Types" >:: test_type_of_app_invalid_args;
      ]
    ];
    "Top-Level Statements" >::: [

    ];
    "File" >::: [

    ]
  ]

(* Suite *)

let suite =
  "Annotated Abstract Syntax Tree" >::: [
    test_constructor;
    test_annotate;
    test_loc;
    test_pp;
    test_type_of;
  ]
