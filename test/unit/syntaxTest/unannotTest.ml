open OUnit2
open Nile.Common
open Nile.Syntax
open CommonTest

(* Assertions *)

let rec assert_expr_equal ~ctxt expected actual = match expected, actual with
  | Unannot.Bool (loc, b), Unannot.Bool (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean literals are not equal"
  | Unannot.Int (loc, i), Unannot.Int (loc', i') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Integer literals are not equal"
  | Unannot.UnOp (loc, op, r), Unannot.UnOp (loc', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    OpTest.assert_un_equal ~ctxt op op';
    assert_expr_equal ~ctxt r r'
  | Unannot.BinOp (loc, l, op, r), Unannot.BinOp (loc', l', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    OpTest.assert_bin_equal ~ctxt op op';
    assert_expr_equal ~ctxt l l';
    assert_expr_equal ~ctxt r r'
  | Unannot.Let (loc, b, rest), Unannot.Let (loc', b', rest') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b';
    assert_expr_equal ~ctxt rest rest'
  | Unannot.LetRec (loc, bs, rest), Unannot.LetRec (loc', bs', rest') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs';
    assert_expr_equal ~ctxt rest rest'
  | Unannot.Abs (loc, id, ty, res, expr), Unannot.Abs (loc', id', ty', res', expr') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~msg:"Parameter identifiers are not equal" id id';
    TypeTest.assert_type_equal ~ctxt ty ty';
    let _ = match res, res' with
      | Some res, Some res' -> TypeTest.assert_type_equal ~ctxt res res';
      | Some res, None -> assert_failure "Expected has type annotation, actual does not"
      | None, Some res -> assert_failure "Expected does not have a type annotation, actual does"
      | None, None -> ()
    in
    assert_expr_equal ~ctxt expr expr'
  | Unannot.App (loc, f, x), Unannot.App (loc', f', x') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt f f';
    assert_expr_equal ~ctxt x x'
  | Unannot.Var (loc, id), Unannot.Var (loc', id') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Variable identifiers are not equal" id id'
  | Unannot.If (loc, c, t, f), Unannot.If (loc', c', t', f') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt c c';
    assert_expr_equal ~ctxt t t';
    assert_expr_equal ~ctxt f f'
  | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Unannot.pp_expr) ~msg:"Expressions are not equal" expected actual
and assert_bindings_equal ~ctxt bs bs' = List.iter2 (assert_binding_equal ~ctxt) bs bs'
and assert_binding_equal ~ctxt (loc, id, ty, expr) (loc', id', ty', expr') =
  LocTest.assert_loc_equal ~ctxt loc loc';
  assert_equal ~ctxt ~printer:Fun.id ~msg:"Bound identifier are not equal" id id';
  let _ = match ty, ty' with
    | Some ty, Some ty' -> TypeTest.assert_type_equal ~ctxt ty ty';
    | Some ty, None -> assert_failure "Expected has type annotation, actual does not"
    | None, Some ty -> assert_failure "Expected does not have a type annotation, actual does"
    | None, None -> ()
  in
  assert_expr_equal ~ctxt expr expr'

let assert_top_equal ~ctxt expected actual = match expected, actual with
  | Unannot.TopLet (loc, b), Unannot.TopLet (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b'
  | Unannot.TopRec (loc, bs), Unannot.TopRec (loc', bs') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs'
  | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Unannot.pp_top) ~msg:"Top-level bindings are not equal" expected actual

let assert_file_equal ~ctxt expected actual = List.iter2 (assert_top_equal ~ctxt) expected actual

(* Constructors *)

let assert_constructor_failure ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Unannot.pp_expr) ~msg:"Expressions are not equal" expected actual

let test_bool ctxt =
  let assert_bool b =
    let loc = LocTest.gen () in
    let expected = Unannot.bool loc b in
    match expected with
      | Unannot.Bool (loc', b') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean values are not equal"
      | actual -> assert_constructor_failure ~ctxt expected actual
  in
  assert_bool true;
  assert_bool false

let test_int ctxt =
  let assert_int i =
    let loc = LocTest.gen () in
    let expected = Unannot.int loc i in
    match expected with
      | Unannot.Int (loc', i') ->
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
    let expected = Unannot.var loc id in
    match expected with
      | Unannot.Var (loc', id') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Variable names are not equal"
      | actual -> assert_constructor_failure ~ctxt expected actual
  in
  assert_var "f";
  assert_var "x"

let assert_un_op ~ctxt op =
  let loc = LocTest.gen () in
  let r = Unannot.int LocTest.dummy 1 in
  let expected = Unannot.un_op loc op r in
  match expected with
    | Unannot.UnOp (loc', op', r') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      OpTest.assert_un_equal ~ctxt op op';
      assert_expr_equal ~ctxt r r'
    | actual -> assert_constructor_failure ~ctxt expected actual

let test_un_op ctxt = assert_un_op ~ctxt Op.un_not

let assert_bin_op ~ctxt op =
  let loc = LocTest.gen () in
  let l = Unannot.int LocTest.dummy 1 in
  let r = Unannot.int LocTest.dummy 2 in
  let expected = Unannot.bin_op loc l op r in
  match expected with
    | Unannot.BinOp (loc', l', op', r') ->
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
    let expected = Unannot.cond loc c t f in
    match expected with
      | Unannot.If (loc', c', t', f') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_expr_equal ~ctxt c c';
        assert_expr_equal ~ctxt t t';
        assert_expr_equal ~ctxt f f'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let c = Unannot.bool LocTest.dummy true in
  let t = Unannot.int LocTest.dummy 1 in
  let f = Unannot.int LocTest.dummy 2 in
  assert_cond c t f

let id = "id-one"
let ty = Type.int
let x = Unannot.int LocTest.dummy 1
let b = Unannot.binding LocTest.dummy id ty x

let id' = "id-two"
let ty' = Type.bool
let x' = Unannot.bool LocTest.dummy true
let b' = Unannot.binding LocTest.dummy id' ty' x'

let test_bind ctxt =
  let assert_bind b rest =
    let loc = LocTest.gen () in
    let expected = Unannot.bind loc b rest in
    match expected with
      | Unannot.Let (loc', b', rest') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_binding_equal ~ctxt b b';
        assert_expr_equal ~ctxt rest rest'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  Unannot.var LocTest.dummy id
    |> assert_bind b

let test_bind_rec ctxt =
  let assert_bind bs rest =
    let loc = LocTest.gen () in
    let expected = Unannot.bind_rec loc bs rest in
    match expected with
      | Unannot.LetRec (loc', bs', rest') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_bindings_equal ~ctxt bs bs';
        assert_expr_equal ~ctxt rest rest'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let l = Unannot.var LocTest.dummy id in
  let r = Unannot.var LocTest.dummy id' in
  Unannot.bin_op LocTest.dummy l Op.bin_eq r
    |> assert_bind [b; b']

let test_abs ctxt =
  let assert_abs id ty res body =
    let loc = LocTest.gen () in
    let expected = Unannot.abs loc id ty res body in
    match expected with
      | Unannot.Abs (loc', id', ty', res', body') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_equal ~ctxt ~printer:Fun.id ~msg:"Bound function identifiers are not equal" id id';
        TypeTest.assert_type_equal ~ctxt ty ty';
        TypeTest.assert_type_equal ~ctxt res res';
        assert_expr_equal ~ctxt body body'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let var = Unannot.var LocTest.dummy id in
  Unannot.bin_op LocTest.dummy var Op.bin_eq var
    |> assert_abs id ty ty'

let test_app ctxt =
  let assert_app f x =
    let loc = LocTest.gen () in
    let expected = Unannot.app loc f x in
    match expected with
      | Unannot.App (loc', f', x') ->
        LocTest.assert_loc_equal ~ctxt loc loc';
        assert_expr_equal ~ctxt f f';
        assert_expr_equal ~ctxt x x'
      | actual -> assert_constructor_failure ~ctxt expected actual
  in

  let f = Unannot.var LocTest.dummy id in
  Unannot.var LocTest.dummy id'
    |> assert_app f

let test_top_bind ctxt =
  let loc = LocTest.gen () in
  let top = Unannot.top_bind loc b in
  match top with
    | Unannot.TopLet (loc', b') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      assert_binding_equal ~ctxt b b'
    | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Unannot.pp_top) ~msg:"Top-level bindings are not equal" top top'

let test_top_bind_rec ctxt =
  let loc = LocTest.gen () in
  let bs = [b; b'] in
  let top = Unannot.top_bind_rec loc bs in
  match top with
    | Unannot.TopRec (loc', bs') ->
      LocTest.assert_loc_equal ~ctxt loc loc';
      assert_bindings_equal ~ctxt bs bs'
    | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Unannot.pp_top) ~msg:"Top-level bindings are not equal" top top'

let test_file ctxt =
  let expected =
    let b =
      let ty = Type.int in
      Unannot.int LocTest.dummy 1
        |> Unannot.binding LocTest.dummy "id-one" ty
    in
    let b' =
      let ty = Type.bool in
      Unannot.bool LocTest.dummy true
        |> Unannot.binding LocTest.dummy "id-two" ty
    in
    Unannot.top_bind_rec LocTest.dummy [b; b']
  in
  let expected' =
    let ty = Type.bool in
    Unannot.bool LocTest.dummy true
      |> Unannot.binding LocTest.dummy "id-two" ty
      |> Unannot.top_bind LocTest.dummy
  in

  let file = Unannot.file [expected; expected'] in
  match file with
      | actual :: actual' :: [] ->
        assert_top_equal ~ctxt expected actual;
        assert_top_equal ~ctxt expected' actual'
      | actual -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Unannot.pp_file) ~msg:"Source files are not equal" file actual

let test_constructor =
  "Constructors" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Boolean"  >:: test_bool;
        "Integer"  >:: test_int;
        "Variable" >:: test_var;
      ];
      "Operators" >::: [
        "Unary" >::: [
          "Logical Not" >:: test_not;
        ];
        "Binary" >::: [
          "Addition"              >:: test_add;
          "Subtraction"           >:: test_sub;
          "Multiplication"        >:: test_mul;
          "Division"              >:: test_div;
          "Modulus"               >:: test_mod;
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
      "Value Binding"           >:: test_top_bind;
      "Recursive Value Binding" >:: test_top_bind_rec;
    ];
    "Files" >:: test_file;
  ]

(* Location Tracking *)

let assert_loc_expr ~ctxt loc ast =
  ast
    |> Unannot.loc_expr
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc_bool ctxt =
  let loc = LocTest.gen () in
  Unannot.bool loc true
    |> assert_loc_expr ~ctxt loc

let test_loc_int ctxt =
  let loc = LocTest.gen () in
  Unannot.int loc 1
    |> assert_loc_expr ~ctxt loc

let test_loc_var ctxt =
  let loc = LocTest.gen () in
  Unannot.var loc "x"
    |> assert_loc_expr ~ctxt loc

let test_loc_un_op ctxt =
  let loc = LocTest.gen () in
  Unannot.bool LocTest.dummy true
    |> Unannot.un_op loc Op.un_not
    |> assert_loc_expr ~ctxt loc

let test_loc_bin_op ctxt =
  let loc = LocTest.gen () in
  let l = Unannot.int LocTest.dummy 1 in
  let r = Unannot.int LocTest.dummy 2 in
  Unannot.bin_op loc l Op.bin_add r
    |> assert_loc_expr ~ctxt loc

let test_loc_cond ctxt =
  let loc = LocTest.gen () in
  let c = Unannot.bool LocTest.dummy true in
  let t = Unannot.int LocTest.dummy 1 in
  let f = Unannot.int LocTest.dummy 2 in
  Unannot.cond loc c t f
    |> assert_loc_expr ~ctxt loc

let test_loc_bind ctxt =
  let loc = LocTest.gen () in
  let b =
    Unannot.int LocTest.dummy 1
      |> Unannot.binding LocTest.dummy "x" Type.int
  in
  Unannot.var LocTest.dummy "x"
    |> Unannot.bind loc b
    |> assert_loc_expr ~ctxt loc

let test_loc_bind_rec ctxt =
  let loc = LocTest.gen () in
  let bs = [
    Unannot.int LocTest.dummy 1
      |> Unannot.binding LocTest.dummy "x" Type.int;
    Unannot.int LocTest.dummy 2
      |> Unannot.binding LocTest.dummy "y" Type.int
  ] in
  let x = Unannot.var LocTest.dummy "x" in
  let y = Unannot.var LocTest.dummy "y" in
  Unannot.bin_op LocTest.dummy x Op.bin_eq y
    |> Unannot.bind_rec loc bs
    |> assert_loc_expr ~ctxt loc

let test_loc_abs ctxt =
  let loc = LocTest.gen () in
  Unannot.int LocTest.dummy 1
    |> Unannot.abs loc "param" Type.int Type.int
    |> assert_loc_expr ~ctxt loc

let test_loc_app ctxt =
  let loc = LocTest.gen () in
  let f = Unannot.var LocTest.dummy "f" in
  Unannot.var LocTest.dummy "x"
    |> Unannot.app loc f
    |> assert_loc_expr ~ctxt loc

let test_loc_top_bind ctxt =
  let loc = LocTest.gen () in
  Unannot.top_bind loc b
    |> Unannot.loc_top
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc_top_bind_rec ctxt =
  let loc = LocTest.gen () in
  Unannot.top_bind_rec loc [b; b']
    |> Unannot.loc_top
    |> LocTest.assert_loc_equal ~ctxt loc

let test_loc =
  "Location Information" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Boolean"              >:: test_loc_bool;
        "Integer"              >:: test_loc_int;
        "Variable"             >:: test_loc_var;
      ];
      "Operators" >::: [
        "Unary Operator"       >:: test_loc_un_op;
        "Binary Operator"      >:: test_loc_bin_op;
      ];
      "Conditional"          >:: test_loc_cond;
      "Bindings"             >:: test_loc_bind;
      "Recursive Bindings"   >:: test_loc_bind_rec;
      "Function Abstraction" >:: test_loc_abs;
      "Function Application" >:: test_loc_app;
    ];
    "Top-Level Statements" >::: [
      "Value Binding"            >:: test_loc_top_bind;
      "Recursive Value Bindings" >:: test_loc_top_bind_rec;
    ]
  ]

(* Pretty Printing *)

let assert_pp_expr = CommonTest.Util.assert_pp Unannot.pp_expr

let id_f = "f"
let id_w = "w"
let id_x = "x"
let id_y = "y"
let id_z = "z"
let id_temp_one = "temporaryVariableOne"
let id_temp_two = "temporaryVariableTwo"

let var_f = Unannot.var LocTest.dummy id_f
let var_w = Unannot.var LocTest.dummy id_w
let var_x = Unannot.var LocTest.dummy id_x
let var_y = Unannot.var LocTest.dummy id_y
let var_z = Unannot.var LocTest.dummy id_z
let var_temp_one = Unannot.var LocTest.dummy id_temp_one
let var_temp_two = Unannot.var LocTest.dummy id_temp_two

let expr_temp_one = Unannot.bin_op LocTest.dummy var_w Op.bin_add var_x
let expr_temp_two = Unannot.bin_op LocTest.dummy var_y Op.bin_add var_z
let expr_result = Unannot.bin_op LocTest.dummy var_temp_one Op.bin_add var_temp_two

let b_temp_one = Unannot.binding LocTest.dummy id_temp_one Type.int expr_temp_one
let b_temp_two = Unannot.binding LocTest.dummy id_temp_two Type.int expr_temp_two

let temp_two = Unannot.bind LocTest.dummy b_temp_two expr_result
let temp_one = Unannot.bind LocTest.dummy b_temp_one temp_two

let test_pp_bool ctxt =
  Unannot.bool LocTest.dummy true
    |> assert_pp_expr ~ctxt  ["true"];
  Unannot.bool LocTest.dummy false
    |> assert_pp_expr ~ctxt ["false"]

let test_pp_int ctxt =
  Unannot.int LocTest.dummy 1
    |> assert_pp_expr ~ctxt ["1"];
  Unannot.int LocTest.dummy 42
    |> assert_pp_expr ~ctxt ["42"];
  Unannot.int LocTest.dummy (-10)
    |> assert_pp_expr ~ctxt ["-10"]

let test_pp_var ctxt =
  Unannot.var LocTest.dummy "variableName"
    |> assert_pp_expr ~ctxt ["variableName"]

let test_pp_un_op_constant ctxt =
  Unannot.bool LocTest.dummy true
    |> Unannot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!true"]

let test_pp_un_op_equal_precedence ctxt =
  Unannot.bool LocTest.dummy true
    |> Unannot.un_op LocTest.dummy Op.un_not
    |> Unannot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!!true"]

let test_pp_un_op_higher_precedence ctxt =
  Unannot.bool LocTest.dummy true
    |> Unannot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!true"]

let test_pp_un_op_lower_precedence ctxt =
  Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 1) Op.bin_eq (Unannot.int LocTest.dummy 2)
    |> Unannot.un_op LocTest.dummy Op.un_not
    |> assert_pp_expr ~ctxt ["!(1 == 2)"]

let test_pp_bin_op_constant ctxt =
  Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 1) Op.bin_add (Unannot.int LocTest.dummy 2)
    |> assert_pp_expr ~ctxt ["1 + 2"]

let test_pp_bin_op_equal_precedence ctxt =
  let lhs = Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 1) Op.bin_add (Unannot.int LocTest.dummy 2) in
  let rhs = Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 3) Op.bin_add (Unannot.int LocTest.dummy 4) in
  Unannot.bin_op LocTest.dummy lhs Op.bin_sub rhs
    |> assert_pp_expr ~ctxt ["1 + 2 - 3 + 4"];
  rhs
    |> Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 2) Op.bin_sub
    |> Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 1) Op.bin_add
    |> assert_pp_expr ~ctxt ["1 + 2 - 3 + 4"]

let test_pp_bin_op_higher_precedence ctxt =
  let lhs = Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 1) Op.bin_mul (Unannot.int LocTest.dummy 2) in
  let rhs = Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 3) Op.bin_mul (Unannot.int LocTest.dummy 4) in
  let const = Unannot.int LocTest.dummy 5 in

  Unannot.bin_op LocTest.dummy lhs Op.bin_add const
    |> assert_pp_expr ~ctxt ["1 * 2 + 5"];
  Unannot.bin_op LocTest.dummy const Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["5 + 3 * 4"];
  Unannot.bin_op LocTest.dummy lhs Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["1 * 2 + 3 * 4"]

let test_pp_bin_op_lower_precedence ctxt =
  let lhs = Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 1) Op.bin_eq (Unannot.int LocTest.dummy 2) in
  let rhs = Unannot.bin_op LocTest.dummy (Unannot.int LocTest.dummy 3) Op.bin_eq (Unannot.int LocTest.dummy 4) in
  let const = Unannot.int LocTest.dummy 5 in

  Unannot.bin_op LocTest.dummy lhs Op.bin_add const
    |> assert_pp_expr ~ctxt ["(1 == 2) + 5"];
  Unannot.bin_op LocTest.dummy const Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["5 + (3 == 4)"];
  Unannot.bin_op LocTest.dummy lhs Op.bin_add rhs
    |> assert_pp_expr ~ctxt ["(1 == 2) + (3 == 4)"]

let test_pp_bin_op_long ctxt =
  let name = String.make 50 'a' in
  let var = Unannot.var LocTest.dummy name in
  Unannot.bin_op LocTest.dummy var Op.bin_add var
    |> assert_pp_expr ~ctxt [
         name ^ " +";
         "  " ^ name;
       ]

let test_pp_cond_one_line ctxt =
  let c = Unannot.var LocTest.dummy "c" in
  let t = Unannot.var LocTest.dummy "t" in
  let f = Unannot.var LocTest.dummy "f" in

  Unannot.cond LocTest.dummy c t f
    |> assert_pp_expr ~ctxt ["if c then t else f"]

let test_pp_cond_three_line ctxt =
  let c = Unannot.var LocTest.dummy "conditionIsAVeryLongExpression" in
  let t = Unannot.var LocTest.dummy "trueBranchIsAVeryLongExpression" in
  let f = Unannot.var LocTest.dummy "falseBranchIsAVeryLongExpression" in

  Unannot.cond LocTest.dummy c t f
    |> assert_pp_expr ~ctxt [
         "if conditionIsAVeryLongExpression";
         "then trueBranchIsAVeryLongExpression";
         "else falseBranchIsAVeryLongExpression";
       ]

let test_pp_cond_full_spread ctxt =
  let c = Unannot.var LocTest.dummy "conditionIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in
  let t = Unannot.var LocTest.dummy "trueBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in
  let f = Unannot.var LocTest.dummy "falseBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in

  Unannot.cond LocTest.dummy c t f
    |> assert_pp_expr ~ctxt [
         "if";
         "  conditionIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
         "then";
         "  trueBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
         "else";
         "  falseBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
       ]

let test_pp_bind_simple_var ctxt =
  let b = Unannot.binding LocTest.dummy id_x Type.int (Unannot.int LocTest.dummy 1) in
  Unannot.bind LocTest.dummy b var_x
    |> assert_pp_expr ~ctxt [
         "let x: Int = 1 in";
         "x";
       ]

let test_pp_bind_compound_var ctxt =
  let id_final = "finalVariable" in
  let v_final = Unannot.var LocTest.dummy id_final in
  let b_final = Unannot.binding LocTest.dummy id_final Type.int temp_one in

  Unannot.bind LocTest.dummy b_final v_final
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
    Unannot.abs LocTest.dummy id_x Type.int Type.int expr_temp_one
      |> Unannot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int)
  in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Unannot.binding LocTest.dummy id_f ty fn in

  Unannot.bind LocTest.dummy b var_f
    |> assert_pp_expr ~ctxt [
         "let f(w: Int, x: Int): Int = w + x in";
         "f";
       ]

let test_pp_bind_compound_function ctxt =
  let body = Unannot.bind LocTest.dummy b_temp_one temp_two in
  let fn =
    Unannot.abs LocTest.dummy id_z Type.int Type.int body
      |> Unannot.abs LocTest.dummy id_y Type.int (Type.func Type.int Type.int)
      |> Unannot.abs LocTest.dummy id_x Type.int (Type.func Type.int (Type.func Type.int Type.int))
      |> Unannot.abs LocTest.dummy id_w Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int)))
  in
  let ty = Type.func Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int))) in
  let b = Unannot.binding LocTest.dummy id_f ty fn in

  Unannot.bind LocTest.dummy b var_f
    |> assert_pp_expr ~ctxt [
         "let f(w: Int, x: Int, y: Int, z: Int): Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "f";
       ]

let test_pp_bind_complex_result ctxt =
  let fn = Unannot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int) expr_temp_one in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Unannot.binding LocTest.dummy id_f ty fn in

  Unannot.bind LocTest.dummy b var_f
    |> assert_pp_expr ~ctxt [
         "let f(w: Int): Int -> Int = w + x in";
         "f";
       ]

let test_pp_bind_rec_simple_var ctxt =
  let b_x = Unannot.binding LocTest.dummy id_x Type.int var_y in
  let b_y = Unannot.binding LocTest.dummy id_y Type.int var_x in
  let res = Unannot.bin_op LocTest.dummy var_x Op.bin_add var_y in

  Unannot.bind_rec LocTest.dummy [b_x] res
    |> assert_pp_expr ~ctxt [
         "let rec x: Int = y in";
         "x + y";
       ];
  Unannot.bind_rec LocTest.dummy [b_x; b_y] res
    |> assert_pp_expr ~ctxt [
         "let rec x: Int = y and y: Int = x in";
         "x + y";
       ]

let test_pp_bind_rec_compound_var ctxt =
  let id_final = "finalVariable" in
  let v_final = Unannot.var LocTest.dummy id_final in
  let b_final = Unannot.binding LocTest.dummy id_final Type.int temp_one in

  Unannot.bind_rec LocTest.dummy [b_final] v_final
    |> assert_pp_expr ~ctxt [
         "let rec finalVariable: Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "finalVariable";
       ];
  Unannot.bind_rec LocTest.dummy [b_final; b_final] v_final
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
    Unannot.abs LocTest.dummy id_x Type.int Type.int expr_temp_one
      |> Unannot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int)
  in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Unannot.binding LocTest.dummy id_f ty fn in

  Unannot.bind_rec LocTest.dummy [b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int): Int = w + x in";
         "f";
       ];
  Unannot.bind_rec LocTest.dummy [b; b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int): Int = w + x and f(w: Int, x: Int): Int = w + x in";
         "f";
       ]

let test_pp_bind_rec_compound_function ctxt =
  let body = Unannot.bind LocTest.dummy b_temp_one temp_two in
  let fn =
    Unannot.abs LocTest.dummy id_z Type.int Type.int body
      |> Unannot.abs LocTest.dummy id_y Type.int (Type.func Type.int Type.int)
      |> Unannot.abs LocTest.dummy id_x Type.int (Type.func Type.int (Type.func Type.int Type.int))
      |> Unannot.abs LocTest.dummy id_w Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int)))
  in
  let ty = Type.func Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int))) in
  let b = Unannot.binding LocTest.dummy id_f ty fn in

  Unannot.bind_rec LocTest.dummy [b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int, x: Int, y: Int, z: Int): Int =";
         "  let temporaryVariableOne: Int = w + x in";
         "  let temporaryVariableTwo: Int = y + z in";
         "  temporaryVariableOne + temporaryVariableTwo";
         "in";
         "f";
       ];
  Unannot.bind_rec LocTest.dummy [b; b] var_f
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
  let fn = Unannot.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int) expr_temp_one in
  let ty = Type.func Type.int (Type.func Type.int Type.int) in
  let b = Unannot.binding LocTest.dummy id_f ty fn in

  Unannot.bind_rec LocTest.dummy [b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int): Int -> Int = w + x in";
         "f";
       ];
  Unannot.bind_rec LocTest.dummy [b; b] var_f
    |> assert_pp_expr ~ctxt [
         "let rec f(w: Int): Int -> Int = w + x and f(w: Int): Int -> Int = w + x in";
         "f";
       ]

let test_pp_app_named ctxt =
  let x = Unannot.int LocTest.dummy 1 in
  let y = Unannot.bool LocTest.dummy true in

  let app_one = Unannot.app LocTest.dummy var_f x in
  Unannot.app LocTest.dummy app_one y
    |> assert_pp_expr ~ctxt ["f 1 true"]

let test_pp_app_literal ctxt =
  let f =
    Unannot.bin_op LocTest.dummy var_x Op.bin_eq var_y
      |> Unannot.abs LocTest.dummy id_y Type.int Type.bool
      |> Unannot.abs LocTest.dummy id_x Type.int (Type.func Type.int Type.bool)
  in
  let x = Unannot.int LocTest.dummy 1 in
  let y = Unannot.bool LocTest.dummy true in

  let app_one = Unannot.app LocTest.dummy f x in
  Unannot.app LocTest.dummy app_one y
    |> assert_pp_expr ~ctxt ["((x: Int, y: Int): Bool => x == y) 1 true"]

let test_pp_app_func_result ctxt =
  let f = Unannot.app LocTest.dummy var_f var_x in
  let x = Unannot.int LocTest.dummy 1 in
  let y = Unannot.bool LocTest.dummy true in

  let app_one = Unannot.app LocTest.dummy f x in
  Unannot.app LocTest.dummy app_one y
    |> assert_pp_expr ~ctxt ["(f x) 1 true"]

let test_pp_app_parenthesized ctxt =
  let x =
    let x = Unannot.int LocTest.dummy 1 in
    let y = Unannot.int LocTest.dummy 2 in
    Unannot.bin_op LocTest.dummy x Op.bin_add y
  in
  let y =
    let x = Unannot.bool LocTest.dummy true in
    let y = Unannot.bool LocTest.dummy false in
    Unannot.bin_op LocTest.dummy x Op.bin_and y
  in
  let z =
    let g = Unannot.var LocTest.dummy "g" in
    let z = Unannot.var LocTest.dummy "z" in
    Unannot.app LocTest.dummy g z
  in

  let app_one = Unannot.app LocTest.dummy var_f x in
  let app_two = Unannot.app LocTest.dummy app_one y in
  Unannot.app LocTest.dummy app_two z
    |> assert_pp_expr ~ctxt ["f (1 + 2) (true && false) (g z)"]

let test_pp_app_wrap ctxt =
  let f = Unannot.var LocTest.dummy "aReallyLongFunctionName" in
  let x = Unannot.var LocTest.dummy "aLongParameterName" in
  let y = Unannot.var LocTest.dummy "yetAnotherReallyLongParameterName" in
  let z = Unannot.var LocTest.dummy "whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames" in

  let app_one = Unannot.app LocTest.dummy f x in
  let app_two = Unannot.app LocTest.dummy app_one y in
  Unannot.app LocTest.dummy app_two z
    |> assert_pp_expr ~ctxt [
         "aReallyLongFunctionName aLongParameterName yetAnotherReallyLongParameterName";
         "  whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames";
       ]

let assert_pp_top = CommonTest.Util.assert_pp Unannot.pp_top

let test_pp_top_bind ctxt =
  Unannot.top_bind LocTest.dummy b
    |> assert_pp_top ~ctxt [
         "let id-one: Int = 1";
       ]

let test_pp_top_bind_rec ctxt =
  Unannot.top_bind_rec LocTest.dummy [b; b']
    |> assert_pp_top ~ctxt [
         "let rec id-one: Int = 1";
         "and id-two: Bool = true"
       ]

let assert_pp_file = CommonTest.Util.assert_pp Unannot.pp_file

let test_pp_file ctxt =
  let expected =
    let b =
      let ty = Type.int in
      Unannot.int LocTest.dummy 1
        |> Unannot.binding LocTest.dummy "id-one" ty
    in
    let b' =
      let ty = Type.bool in
      Unannot.bool LocTest.dummy true
        |> Unannot.binding LocTest.dummy "id-two" ty
    in
    Unannot.top_bind_rec LocTest.dummy [b; b']
  in
  let expected' =
    let ty = Type.bool in
    Unannot.bool LocTest.dummy true
      |> Unannot.binding LocTest.dummy "id-two" ty
      |> Unannot.top_bind LocTest.dummy
  in

  Unannot.file [expected; expected']
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

(* Suite *)

let suite =
  "Unannotated Abstract Syntax Tree" >::: [
    test_constructor;
    test_loc;
    test_pp;
  ]
