open Format
open OUnit2
open Common
open Syntax

let rec assert_expr_equal ~ctxt expected actual = match expected, actual with
  | Ast.Bool (loc, b), Ast.Bool (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean literals are not equal"
  | Ast.Int (loc, i), Ast.Int (loc', i') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Integer literals are not equal"
  | Ast.UnOp (loc, op, r), Ast.UnOp (loc', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    CommonTest.OpTest.assert_un_equal ~ctxt op op';
    assert_expr_equal ~ctxt r r'
  | Ast.BinOp (loc, l, op, r), Ast.BinOp (loc', l', op', r') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    CommonTest.OpTest.assert_bin_equal ~ctxt op op';
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
  | Ast.Abs (loc, ps, ty, expr), Ast.Abs (loc', ps', ty', expr') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_params_equal ~ctxt ps ps';
    CommonTest.TypeTest.assert_type_equal ~ctxt ty ty';
    assert_expr_equal ~ctxt expr expr'
  | Ast.App (loc, f, xs), Ast.App (loc', f', xs') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt f f';
    let iter (x, x') = assert_expr_equal ~ctxt x x' in
    xs'
      |> List.combine xs
      |> List.iter iter
  | Ast.Var (loc, id), Ast.Var (loc', id') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Variable identifiers are not equal"
  | Ast.If (loc, c, t, f), Ast.If (loc', c', t', f') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_expr_equal ~ctxt c c';
    assert_expr_equal ~ctxt t t';
    assert_expr_equal ~ctxt f f'
  | expected, actual -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Ast.pp_expr) ~msg:"Expressions are not equal" expected actual
and assert_bindings_equal ~ctxt bs bs' =
  let iter (b, b') = assert_binding_equal ~ctxt b b' in
  bs'
    |> List.combine bs
    |> List.iter iter
and assert_binding_equal ~ctxt (loc, id, ty, expr) (loc', id', ty', expr') =
  LocTest.assert_loc_equal ~ctxt loc loc';
  assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Bound identifier are not equal";
  CommonTest.TypeTest.assert_type_equal ~ctxt ty ty';
  assert_expr_equal ~ctxt expr expr'
and assert_params_equal ~ctxt ps ps' =
  let iter (p, p') = assert_param_equal ~ctxt p p' in
  ps'
    |> List.combine ps
    |> List.iter iter
and assert_param_equal ~ctxt (loc, id, ty) (loc', id', ty') =
  LocTest.assert_loc_equal ~ctxt loc loc';
  assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Parameter names are not equal";
  CommonTest.TypeTest.assert_type_equal ~ctxt ty ty'

let assert_top_equal ~ctxt expected actual = match expected, actual with
  | Ast.TopLet (loc, b), Ast.TopLet (loc', b') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_binding_equal ~ctxt b b'
  | Ast.TopRec (loc, bs), Ast.TopRec (loc', bs') ->
    LocTest.assert_loc_equal ~ctxt loc loc';
    assert_bindings_equal ~ctxt bs bs'
  | expected, actual -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Ast.pp_top) ~msg:"Top-level bindings are not equal" expected actual

let assert_file_equal ~ctxt expected actual =
  let iter (expected, actual) = assert_top_equal ~ctxt expected actual in
  actual
    |> List.combine expected
    |> List.iter iter

let assert_pp pp ~ctxt lines ast =
  let printer str =
    let map str = sprintf "%S" str in
    str
      |> String.split_on_char '\n'
      |> List.map map
      |> String.concat "\n"
  in
  let expected = String.concat "\n" lines in
  fprintf str_formatter "%t" (pp ast)
    |> flush_str_formatter
    |> assert_equal ~ctxt ~printer expected

let suite =
  let test_expr =
    let test_constructor =
      let cmp = CommonTest.Util.never in
      let msg = "Abstract syntax trees are not equal" in
      let printer = CommonTest.Util.printer Ast.pp_expr in

      let test_primitive =
        let test_bool ctxt =
          let assert_bool ~ctxt loc b = function
            | Ast.Bool (loc', b') ->
              LocTest.assert_loc_equal ~ctxt loc loc';
              assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean values are not equal"
            | actual -> assert_equal ~ctxt ~cmp ~printer ~msg (Ast.bool loc b) actual
          in

          Ast.bool LocTest.dummy true
            |> assert_bool ~ctxt LocTest.dummy true;
          Ast.bool LocTest.dummy false
            |> assert_bool ~ctxt LocTest.dummy false
        in
        let test_int ctxt =
          let assert_int ~ctxt loc i = function
            | Ast.Int (loc', i') ->
              LocTest.assert_loc_equal ~ctxt loc loc';
              assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Integer values are not equal"
            | actual -> assert_equal ~ctxt ~cmp ~printer ~msg (Ast.int loc i) actual
          in

          Ast.int LocTest.dummy 0
            |> assert_int ~ctxt LocTest.dummy 0;
          Ast.int LocTest.dummy 42
            |> assert_int ~ctxt LocTest.dummy 42;
          Ast.int LocTest.dummy (-42)
            |> assert_int ~ctxt LocTest.dummy (-42)
        in
        let test_var ctxt =
          let assert_var ~ctxt loc id = function
            | Ast.Var (loc', id') ->
              LocTest.assert_loc_equal ~ctxt loc loc';
              assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Variable names are not equal"
            | actual -> assert_equal ~ctxt ~cmp ~printer ~msg (Ast.var loc id) actual
          in

          let id = "id" in
          Ast.var LocTest.dummy id
            |> assert_var ~ctxt LocTest.dummy id
        in
        "Primitives" >::: [
          "Booleans"  >:: test_bool;
          "Integers"  >:: test_int;
          "Variables" >:: test_var;
        ]
      in
      let test_un_op =
        let assert_un_op ~ctxt ~assert_op loc r = function
          | Ast.UnOp (loc', op, r') ->
            assert_op ~ctxt op;
            LocTest.assert_loc_equal ~ctxt loc loc';
            assert_expr_equal ~ctxt r r'
          | _ -> assert_failure "Invalid unary operator"
        in
        (* let assert_invalid_op ~ctxt expected actual =
          assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Op.pp_un) ~msg:"Unexpected unary operator" expected actual
        in *)

        let r = Ast.int LocTest.dummy 1 in

        let test_not ctxt =
          let assert_op ~ctxt op =
            let _ = ctxt in
            match op with
              | Op.Not -> ()
              (* | actual -> assert_invalid_op ~ctxt Op.un_not actual *)
          in

          Ast.un_op LocTest.dummy Op.un_not r
            |> assert_un_op ~ctxt ~assert_op LocTest.dummy r
        in
        "Unary Operators" >::: [
          "Boolean Negation" >:: test_not;
        ]
      in
      let test_operator =
        let test_bin_op =
          let assert_bin_op ~ctxt ~assert_op loc l r = function
            | Ast.BinOp (loc', l', op, r') ->
              assert_op ~ctxt op;
              LocTest.assert_loc_equal ~ctxt loc loc';
              assert_expr_equal ~ctxt l l';
              assert_expr_equal ~ctxt r r'
            | _ -> assert_failure "Invalid binary operator"
          in
          let assert_invalid_op ~ctxt expected actual =
            assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Op.pp_bin) ~msg:"Unexpected binary operator" expected actual
          in

          let l = Ast.int LocTest.dummy 1 in
          let r = Ast.int LocTest.dummy 2 in

          let test_add ctxt =
            let op = Op.bin_add in
            let assert_op ~ctxt = function
              | Op.Add -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_sub ctxt =
            let op = Op.bin_sub in
            let assert_op ~ctxt = function
              | Op.Sub -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_mul ctxt =
            let op = Op.bin_mul in
            let assert_op ~ctxt = function
              | Op.Mul -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_div ctxt =
            let op = Op.bin_div in
            let assert_op ~ctxt = function
              | Op.Div -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_mod ctxt =
            let op = Op.bin_mod in
            let assert_op ~ctxt = function
              | Op.Mod -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_and ctxt =
            let op = Op.bin_and in
            let assert_op ~ctxt = function
              | Op.And -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_or ctxt =
            let op = Op.bin_or in
            let assert_op ~ctxt = function
              | Op.Or -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_eq ctxt =
            let op = Op.bin_eq in
            let assert_op ~ctxt = function
              | Op.Eq -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_neq ctxt =
            let op = Op.bin_neq in
            let assert_op ~ctxt = function
              | Op.Neq -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_lte ctxt =
            let op = Op.bin_lte in
            let assert_op ~ctxt = function
              | Op.Lte -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_lt ctxt =
            let op = Op.bin_lt in
            let assert_op ~ctxt = function
              | Op.Lt -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_gt ctxt =
            let op = Op.bin_gt in
            let assert_op ~ctxt = function
              | Op.Gt -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
          let test_gte ctxt =
            let op = Op.bin_gte in
            let assert_op ~ctxt = function
              | Op.Gte -> ()
              | actual -> assert_invalid_op ~ctxt op actual
            in

            Ast.bin_op LocTest.dummy l op r
              |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
          in
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
      let test_bind _ = () in
      let test_bind_rec _ = () in
      let test_abs _ = () in
      let test_app _ = () in
      let test_cond =
        "Conditional" >::: [

        ]
      in
      "Constructors" >::: [
        test_primitive;
        test_operator;
        test_cond;
        "Value Binding"            >:: test_bind;
        "Recursive Value Bindings" >:: test_bind_rec;
        "Function Abstraction"     >:: test_abs;
        "Function Application"     >:: test_app;
      ]
    in
    let test_pp =
      let assert_pp = assert_pp Ast.pp_expr in

      let id_f = "f" in
      let id_w = "w" in
      let id_x = "x" in
      let id_y = "y" in
      let id_z = "z" in
      let id_temp_one = "temporaryVariableOne" in
      let id_temp_two = "temporaryVariableTwo" in

      let var_f = Ast.var LocTest.dummy id_f in
      let var_w = Ast.var LocTest.dummy id_w in
      let var_x = Ast.var LocTest.dummy id_x in
      let var_y = Ast.var LocTest.dummy id_y in
      let var_z = Ast.var LocTest.dummy id_z in
      let var_temp_one = Ast.var LocTest.dummy id_temp_one in
      let var_temp_two = Ast.var LocTest.dummy id_temp_two in

      let expr_temp_one = Ast.bin_op LocTest.dummy var_w Op.bin_add var_x in
      let expr_temp_two = Ast.bin_op LocTest.dummy var_y Op.bin_add var_z in
      let expr_result = Ast.bin_op LocTest.dummy var_temp_one Op.bin_add var_temp_two in

      let b_temp_one = Ast.binding LocTest.dummy id_temp_one Type.int expr_temp_one in
      let b_temp_two = Ast.binding LocTest.dummy id_temp_two Type.int expr_temp_two in

      let temp_two = Ast.bind LocTest.dummy b_temp_two expr_result in
      let temp_one = Ast.bind LocTest.dummy b_temp_one temp_two in

      let param_w = Ast.param LocTest.dummy id_w Type.int in
      let param_x = Ast.param LocTest.dummy id_x Type.int in
      let param_y = Ast.param LocTest.dummy id_y Type.int in
      let param_z = Ast.param LocTest.dummy id_z Type.int in

      let test_primitive =
        let test_bool ctxt =
          Ast.bool LocTest.dummy true
            |> assert_pp ~ctxt  ["true"];
          Ast.bool LocTest.dummy false
            |> assert_pp ~ctxt ["false"]
        in
        let test_int ctxt =
          Ast.int LocTest.dummy 1
            |> assert_pp ~ctxt ["1"];
          Ast.int LocTest.dummy 42
            |> assert_pp ~ctxt ["42"];
          Ast.int LocTest.dummy (-10)
            |> assert_pp ~ctxt ["-10"]
        in
        let test_var ctxt =
          Ast.var LocTest.dummy "variableName"
            |> assert_pp ~ctxt ["variableName"]
        in
        "Primitives" >::: [
          "Boolean"  >:: test_bool;
          "Integer"  >:: test_int;
          "Variable" >:: test_var;
        ]
      in
      let test_operator =
        let test_un =
          let test_equal_precedence ctxt =
            Ast.bool LocTest.dummy true
              |> Ast.un_op LocTest.dummy Op.un_not
              |> Ast.un_op LocTest.dummy Op.un_not
              |> assert_pp ~ctxt ["!!true"]
          in
          let test_higher_precedence ctxt =
            Ast.bool LocTest.dummy true
              |> Ast.un_op LocTest.dummy Op.un_not
              |> assert_pp ~ctxt ["!true"]
          in
          let test_lower_precedence ctxt =
            Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 1) Op.bin_eq (Ast.int LocTest.dummy 2)
              |> Ast.un_op LocTest.dummy Op.un_not
              |> assert_pp ~ctxt ["!(1 == 2)"]
          in
          "Unary" >::: [
            "Equal Precedence"  >:: test_equal_precedence;
            "Higher Precedence" >:: test_higher_precedence;
            "Lower Precedence"  >:: test_lower_precedence;
          ]
        in
        let test_bin =
          let test_constant ctxt =
            Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 1) Op.bin_add (Ast.int LocTest.dummy 2)
              |> assert_pp ~ctxt ["1 + 2"]
          in
          let test_equal_precedence ctxt =
            let lhs = Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 1) Op.bin_add (Ast.int LocTest.dummy 2) in
            let rhs = Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 3) Op.bin_add (Ast.int LocTest.dummy 4) in
            Ast.bin_op LocTest.dummy lhs Op.bin_sub rhs
              |> assert_pp ~ctxt ["1 + 2 - 3 + 4"];
            rhs
              |> Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 2) Op.bin_sub
              |> Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 1) Op.bin_add
              |> assert_pp ~ctxt ["1 + 2 - 3 + 4"]
          in
          let test_higher_precedence ctxt =
            let lhs = Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 1) Op.bin_mul (Ast.int LocTest.dummy 2) in
            let rhs = Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 3) Op.bin_mul (Ast.int LocTest.dummy 4) in
            let const = Ast.int LocTest.dummy 5 in

            Ast.bin_op LocTest.dummy lhs Op.bin_add const
              |> assert_pp ~ctxt ["1 * 2 + 5"];
            Ast.bin_op LocTest.dummy const Op.bin_add rhs
              |> assert_pp ~ctxt ["5 + 3 * 4"];
            Ast.bin_op LocTest.dummy lhs Op.bin_add rhs
              |> assert_pp ~ctxt ["1 * 2 + 3 * 4"]
          in
          let test_lower_precedence ctxt =
            let lhs = Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 1) Op.bin_eq (Ast.int LocTest.dummy 2) in
            let rhs = Ast.bin_op LocTest.dummy (Ast.int LocTest.dummy 3) Op.bin_eq (Ast.int LocTest.dummy 4) in
            let const = Ast.int LocTest.dummy 5 in

            Ast.bin_op LocTest.dummy lhs Op.bin_add const
              |> assert_pp ~ctxt ["(1 == 2) + 5"];
            Ast.bin_op LocTest.dummy const Op.bin_add rhs
              |> assert_pp ~ctxt ["5 + (3 == 4)"];
            Ast.bin_op LocTest.dummy lhs Op.bin_add rhs
              |> assert_pp ~ctxt ["(1 == 2) + (3 == 4)"]
          in
          let test_long ctxt =
            let name = String.make 50 'a' in
            let var = Ast.var LocTest.dummy name in
            Ast.bin_op LocTest.dummy var Op.bin_add var
              |> assert_pp ~ctxt [
                   name ^ " +";
                   "  " ^ name;
                 ]
          in
          "Binary" >::: [
            "Constant"          >:: test_constant;
            "Equal Precedence"  >:: test_equal_precedence;
            "Higher Precedence" >:: test_higher_precedence;
            "Lower Precedence"  >:: test_lower_precedence;
            "Long"              >:: test_long;
          ]
        in
        "Operators" >::: [
          test_un;
          test_bin;
        ]
      in
      let test_bind =
        let test_simple_var ctxt =
          let b = Ast.binding LocTest.dummy id_x Type.int (Ast.int LocTest.dummy 1) in
          Ast.bind LocTest.dummy b var_x
            |> assert_pp ~ctxt [
                 "let x: Int = 1 in";
                 "x";
               ]
        in
        let test_compound_var ctxt =
          let id_final = "finalVariable" in
          let v_final = Ast.var LocTest.dummy id_final in
          let b_final = Ast.binding LocTest.dummy id_final Type.int temp_one in

          Ast.bind LocTest.dummy b_final v_final
            |> assert_pp ~ctxt [
                 "let finalVariable: Int =";
                 "  let temporaryVariableOne: Int = w + x in";
                 "  let temporaryVariableTwo: Int = y + z in";
                 "  temporaryVariableOne + temporaryVariableTwo";
                 "in";
                 "finalVariable";
               ]
        in
        let test_simple_function ctxt =
          let fn = Ast.abs LocTest.dummy [param_w; param_x] Type.int expr_temp_one in
          let ty = Type.func Type.int (Type.func Type.int Type.int) in
          let b = Ast.binding LocTest.dummy id_f ty fn in

          Ast.bind LocTest.dummy b var_f
            |> assert_pp ~ctxt [
                 "let f(w: Int, x: Int): Int = w + x in";
                 "f";
               ]
        in
        let test_compound_function ctxt =
          let body = Ast.bind LocTest.dummy b_temp_one temp_two in
          let fn = Ast.abs LocTest.dummy [param_w; param_x; param_y; param_z] Type.int body in
          let ty = Type.func Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int))) in
          let b = Ast.binding LocTest.dummy id_f ty fn in

          Ast.bind LocTest.dummy b var_f
            |> assert_pp ~ctxt [
                 "let f(w: Int, x: Int, y: Int, z: Int): Int =";
                 "  let temporaryVariableOne: Int = w + x in";
                 "  let temporaryVariableTwo: Int = y + z in";
                 "  temporaryVariableOne + temporaryVariableTwo";
                 "in";
                 "f";
               ]
        in
        let test_complex_result ctxt =
          let fn = Ast.abs LocTest.dummy [param_w] (Type.func Type.int Type.int) expr_temp_one in
          let ty = Type.func Type.int (Type.func Type.int Type.int) in
          let b = Ast.binding LocTest.dummy id_f ty fn in

          Ast.bind LocTest.dummy b var_f
            |> assert_pp ~ctxt [
                 "let f(w: Int): Int -> Int = w + x in";
                 "f";
               ]
        in
        "Value Bindings" >::: [
          "Simple Variables"     >:: test_simple_var;
          "Compound Variables"   >:: test_compound_var;
          "Simple Function"      >:: test_simple_function;
          "Compound Function"    >:: test_compound_function;
          "Complex Result Value" >:: test_complex_result;
        ]
      in
      let test_bind_rec =
        let test_simple_var ctxt =
          let b_x = Ast.binding LocTest.dummy id_x Type.int var_y in
          let b_y = Ast.binding LocTest.dummy id_y Type.int var_x in
          let res = Ast.bin_op LocTest.dummy var_x Op.bin_add var_y in

          Ast.bind_rec LocTest.dummy [b_x] res
            |> assert_pp ~ctxt [
                 "let rec x: Int = y in";
                 "x + y";
               ];
          Ast.bind_rec LocTest.dummy [b_x; b_y] res
            |> assert_pp ~ctxt [
                 "let rec x: Int = y and y: Int = x in";
                 "x + y";
               ]
        in
        let test_compound_var ctxt =
          let id_final = "finalVariable" in
          let v_final = Ast.var LocTest.dummy id_final in
          let b_final = Ast.binding LocTest.dummy id_final Type.int temp_one in

          Ast.bind_rec LocTest.dummy [b_final] v_final
            |> assert_pp ~ctxt [
                 "let rec finalVariable: Int =";
                 "  let temporaryVariableOne: Int = w + x in";
                 "  let temporaryVariableTwo: Int = y + z in";
                 "  temporaryVariableOne + temporaryVariableTwo";
                 "in";
                 "finalVariable";
               ];
          Ast.bind_rec LocTest.dummy [b_final; b_final] v_final
            |> assert_pp ~ctxt [
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
        in
        let test_simple_function ctxt =
          let fn = Ast.abs LocTest.dummy [param_w; param_x] Type.int expr_temp_one in
          let ty = Type.func Type.int (Type.func Type.int Type.int) in
          let b = Ast.binding LocTest.dummy id_f ty fn in

          Ast.bind_rec LocTest.dummy [b] var_f
            |> assert_pp ~ctxt [
                 "let rec f(w: Int, x: Int): Int = w + x in";
                 "f";
               ];
          Ast.bind_rec LocTest.dummy [b; b] var_f
            |> assert_pp ~ctxt [
                 "let rec f(w: Int, x: Int): Int = w + x and f(w: Int, x: Int): Int = w + x in";
                 "f";
               ]
        in
        let test_compound_function ctxt =
          let body = Ast.bind LocTest.dummy b_temp_one temp_two in
          let fn = Ast.abs LocTest.dummy [param_w; param_x; param_y; param_z] Type.int body in
          let ty = Type.func Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int))) in
          let b = Ast.binding LocTest.dummy id_f ty fn in

          Ast.bind_rec LocTest.dummy [b] var_f
            |> assert_pp ~ctxt [
                 "let rec f(w: Int, x: Int, y: Int, z: Int): Int =";
                 "  let temporaryVariableOne: Int = w + x in";
                 "  let temporaryVariableTwo: Int = y + z in";
                 "  temporaryVariableOne + temporaryVariableTwo";
                 "in";
                 "f";
               ];
          Ast.bind_rec LocTest.dummy [b; b] var_f
            |> assert_pp ~ctxt [
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
        in
        let test_complex_result ctxt =
          let fn = Ast.abs LocTest.dummy [param_w] (Type.func Type.int Type.int) expr_temp_one in
          let ty = Type.func Type.int (Type.func Type.int Type.int) in
          let b = Ast.binding LocTest.dummy id_f ty fn in

          Ast.bind_rec LocTest.dummy [b] var_f
            |> assert_pp ~ctxt [
                 "let rec f(w: Int): Int -> Int = w + x in";
                 "f";
               ];
          Ast.bind_rec LocTest.dummy [b; b] var_f
            |> assert_pp ~ctxt [
                 "let rec f(w: Int): Int -> Int = w + x and f(w: Int): Int -> Int = w + x in";
                 "f";
               ]
        in
        "Recursive Value Bindings" >::: [
          "Simple Variables"      >:: test_simple_var;
          "Compound Variables"    >:: test_compound_var;
          "Simple Functions"      >:: test_simple_function;
          "Compound Functions"    >:: test_compound_function;
          "Complex Result Values" >:: test_complex_result;
        ]
      in
      let test_abs _ = () in
      let test_app =
        let test_named ctxt =
          let x = Ast.int LocTest.dummy 1 in
          let y = Ast.bool LocTest.dummy true in

          Ast.app LocTest.dummy var_f [x; y]
            |> assert_pp ~ctxt ["f 1 true"]
        in
        let test_literal ctxt =
          let f =
            let ty = Type.bool in
            let body = Ast.bin_op LocTest.dummy var_x Op.bin_eq var_y in
            Ast.abs LocTest.dummy [param_x; param_y] ty body
          in
          let x = Ast.int LocTest.dummy 1 in
          let y = Ast.bool LocTest.dummy true in

          Ast.app LocTest.dummy f [x; y]
            |> assert_pp ~ctxt ["((x: Int, y: Int): Bool => x == y) 1 true"]
        in
        let test_func_result ctxt =
          let f = Ast.app LocTest.dummy var_f [var_x] in
          let x = Ast.int LocTest.dummy 1 in
          let y = Ast.bool LocTest.dummy true in

          Ast.app LocTest.dummy f [x; y]
            |> assert_pp ~ctxt ["(f x) 1 true"]
        in
        let test_parenthesized ctxt =
          let x =
            let x = Ast.int LocTest.dummy 1 in
            let y = Ast.int LocTest.dummy 2 in
            Ast.bin_op LocTest.dummy x Op.bin_add y
          in
          let y =
            let x = Ast.bool LocTest.dummy true in
            let y = Ast.bool LocTest.dummy false in
            Ast.bin_op LocTest.dummy x Op.bin_and y
          in
          let z =
            let g = Ast.var LocTest.dummy "g" in
            let z = Ast.var LocTest.dummy "z" in
            Ast.app LocTest.dummy g [z]
          in

          Ast.app LocTest.dummy var_f [x; y; z]
            |> assert_pp ~ctxt ["f (1 + 2) (true && false) (g z)"];
        in
        let test_wrap ctxt =
          let f = Ast.var LocTest.dummy "aReallyLongFunctionName" in
          let x = Ast.var LocTest.dummy "aLongParameterName" in
          let y = Ast.var LocTest.dummy "yetAnotherReallyLongParameterName" in
          let z = Ast.var LocTest.dummy "whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames" in

          Ast.app LocTest.dummy f [x; y; z]
            |> assert_pp ~ctxt [
                 "aReallyLongFunctionName aLongParameterName yetAnotherReallyLongParameterName";
                 "  whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames";
               ]
        in
        "Function Application" >::: [
          "Named Function"          >:: test_named;
          "Literal Function"        >:: test_literal;
          "Functional Result"       >:: test_func_result;
          "Parenthesized Arguments" >:: test_parenthesized;
          "Argument Wraping"        >:: test_wrap;
        ]
      in
      let test_cond =
        let test_one_line ctxt =
          let c = Ast.var LocTest.dummy "c" in
          let t = Ast.var LocTest.dummy "t" in
          let f = Ast.var LocTest.dummy "f" in

          Ast.cond LocTest.dummy c t f
            |> assert_pp ~ctxt ["if c then t else f"]
        in
        let test_three_line ctxt =
          let c = Ast.var LocTest.dummy "conditionIsAVeryLongExpression" in
          let t = Ast.var LocTest.dummy "trueBranchIsAVeryLongExpression" in
          let f = Ast.var LocTest.dummy "falseBranchIsAVeryLongExpression" in

          Ast.cond LocTest.dummy c t f
            |> assert_pp ~ctxt [
                 "if conditionIsAVeryLongExpression";
                 "then trueBranchIsAVeryLongExpression";
                 "else falseBranchIsAVeryLongExpression";
               ]
        in
        let test_full_spread ctxt =
          let c = Ast.var LocTest.dummy "conditionIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in
          let t = Ast.var LocTest.dummy "trueBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in
          let f = Ast.var LocTest.dummy "falseBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself" in

          Ast.cond LocTest.dummy c t f
            |> assert_pp ~ctxt [
                 "if";
                 "  conditionIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
                 "then";
                 "  trueBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
                 "else";
                 "  falseBranchIsSuchAnExtraordinarilyAstoundinglyLongAndComplexExpressionThatItTakesUpAWholeLineByItself";
               ]
        in
        "Conditional" >::: [
          "One Line"    >:: test_one_line;
          "Three Line"  >:: test_three_line;
          "Full Spread" >:: test_full_spread;
        ]
      in
      "Pretty Printing" >::: [
        test_primitive;
        test_operator;
        test_bind;
        test_bind_rec;
        "Function Abstraction"     >:: test_abs;
        test_app;
        test_cond;
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
        Ast.var loc "id"
          |> assert_loc ~ctxt loc
      in
      let test_un_op ctxt =
        Ast.un_op loc Op.un_not (Ast.int LocTest.dummy 1)
          |> assert_loc ~ctxt loc
      in
      let test_bin_op ctxt =
        Ast.bin_op loc (Ast.int LocTest.dummy 1) Op.bin_add (Ast.int LocTest.dummy 2)
          |> assert_loc ~ctxt loc
      in
      let test_cond ctxt =
        Ast.cond loc (Ast.bool LocTest.dummy true) (Ast.int LocTest.dummy 1) (Ast.int LocTest.dummy 2)
          |> assert_loc ~ctxt loc
      in
      let test_abs ctxt =
        Ast.abs loc [] Type.int (Ast.int LocTest.dummy 1)
          |> assert_loc ~ctxt loc
      in
      let test_app ctxt =
        Ast.app loc (Ast.var LocTest.dummy "f") [(Ast.var LocTest.dummy "x")]
          |> assert_loc ~ctxt loc
      in
      let test_bind ctxt =
        let b = Ast.binding LocTest.dummy "id" Type.int (Ast.int LocTest.dummy 1) in
        Ast.bind loc b (Ast.var LocTest.dummy "id")
          |> assert_loc ~ctxt loc
      in
      let test_bind_rec ctxt =
        let b = Ast.binding LocTest.dummy "id" Type.int (Ast.int LocTest.dummy 1) in
        Ast.bind_rec loc [b; b] (Ast.var LocTest.dummy "id")
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
      test_pp;
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
          | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Ast.pp_top) ~msg:"Top-level bindings are not equal" top top'
      in
      let test_top_bind_rec ctxt =
        let bs = [b; b'] in
        let top = Ast.top_bind_rec loc bs in
        match top with
          | Ast.TopRec (loc', bs') ->
            LocTest.assert_loc_equal ~ctxt loc loc';
            assert_bindings_equal ~ctxt bs bs'
          | top' -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Ast.pp_top) ~msg:"Top-level bindings are not equal" top top'
      in
      "Constructors" >::: [
        "Value Binding"           >:: test_top_bind;
        "Recursive Value Binding" >:: test_top_bind_rec;
      ]
    in
    let test_pp =
      let assert_pp = assert_pp Ast.pp_top in

      let test_top_bind ctxt =
        Ast.top_bind loc b
          |> assert_pp ~ctxt [
               "let id-one: Int = 1";
             ]
      in
      let test_top_bind_rec ctxt =
        Ast.top_bind_rec loc [b; b']
          |> assert_pp ~ctxt [
               "let rec id-one: Int = 1";
               "and id-two: Bool = true"
             ]
      in
      "Pretty-Printing" >::: [
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
      test_pp;
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
        | actual -> assert_equal ~ctxt ~cmp:CommonTest.Util.never ~printer:(CommonTest.Util.printer Ast.pp_file) ~msg:"Source files are not equal" file actual
      in
      "Constructor" >:: test_file;
    in
    let test_pp =
      let assert_pp = assert_pp Ast.pp_file in

      let test_pp ctxt =
        file
          |> assert_pp ~ctxt [
               "let rec id-one: Int = 1";
               "and id-two: Bool = true";
               "";
               "let id-two: Bool = true";
             ]
      in
      "Pretty-Printing" >:: test_pp;
    in
    "Source Files" >::: [
      test_constructor;
      test_pp;
    ]
  in
  "Abstract Syntax" >::: [
    test_expr;
    test_top;
    test_file;
  ]
