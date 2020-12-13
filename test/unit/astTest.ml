open Format
open OUnit2
open Nile

let assert_ast_equal ~ctxt expected actual =
  let cmp _ _ = false in
  let msg = "Abstract syntax trees are not equal" in
  let printer ast =
    ast
      |> Ast.pp
      |> fprintf str_formatter "%t"
      |> flush_str_formatter
  in
  let rec assert_ast_equal expected actual = match expected, actual with
    | Ast.Bool (_, b), Ast.Bool (_, b') ->
      assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Boolean literals are not equal"
    | Ast.Int (_, i), Ast.Int (_, i') -> assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Integer literals are not equal"
    | Ast.UnOp (_, op, r), Ast.UnOp (_, op', r') ->
      OpTest.assert_un_equal ~ctxt op op';
      assert_ast_equal r r'
    | Ast.BinOp (_, l, op, r), Ast.BinOp (_, l', op', r') ->
      OpTest.assert_bin_equal ~ctxt op op';
      assert_ast_equal l l';
      assert_ast_equal r r'
    | Ast.Let (_, b, rest), Ast.Let (_, b', rest') ->
      assert_binding ~ctxt b b';
      assert_ast_equal rest rest'
    | Ast.LetRec (_, bs, rest), Ast.LetRec (_, bs', rest') ->
      assert_bindings ~ctxt bs bs';
      assert_ast_equal rest rest'
    | Ast.Abs (_, ps, ty, expr), Ast.Abs (_, ps', ty', expr') ->
      assert_params ~ctxt ps ps';
      TypeTest.assert_type_equal ~ctxt ty ty';
      assert_ast_equal expr expr'
    | Ast.App (_, f, xs), Ast.App (_, f', xs') ->
      assert_ast_equal f f';
      let iter (x, x') = assert_ast_equal x x' in
      xs'
        |> List.combine xs
        |> List.iter iter
    | Ast.Var (_, id), Ast.Var (_, id') -> assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Variable identifiers are not equal"
    | Ast.If (_, c, t, f), Ast.If (_, c', t', f') ->
      assert_ast_equal c c';
      assert_ast_equal t t';
      assert_ast_equal f f'
    | expected, actual -> assert_equal ~ctxt ~cmp ~printer ~msg expected actual
  and assert_bindings ~ctxt bs bs' =
    let iter (b, b') = assert_binding ~ctxt b b' in
    bs'
      |> List.combine bs
      |> List.iter iter
  and assert_binding ~ctxt (_, id, ty, expr) (_, id', ty', expr') =
    assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Bound identifier are not equal";
    TypeTest.assert_type_equal ~ctxt ty ty';
    assert_ast_equal expr expr'
  and assert_params ~ctxt ps ps' =
    let iter (p, p') = assert_param ~ctxt p p' in
    ps'
      |> List.combine ps
      |> List.iter iter
  and assert_param ~ctxt (_, id, ty) (_, id', ty') =
    assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Parameter names are not equal";
    TypeTest.assert_type_equal ~ctxt ty ty'
  in
  assert_ast_equal expected actual

let suite =
  let test_constructors =
    let cmp _ _ = false in
    let msg = "Abstract syntax trees are not equal" in
    let printer ast =
      ast
        |> Ast.pp
        |> fprintf str_formatter "%t"
        |> flush_str_formatter
    in

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
    let test_un_op =
      let assert_un_op ~ctxt ~assert_op loc r = function
        | Ast.UnOp (loc', op, r') ->
          assert_op ~ctxt op;
          LocTest.assert_loc_equal ~ctxt loc loc';
          assert_ast_equal ~ctxt r r'
        | _ -> assert_failure "Invalid unary operator"
      in
      let assert_invalid_op ~ctxt expected actual =
        let cmp _ _ = false in
        let msg = "Unexpected unary operator" in
        let printer op =
          op
            |> Op.pp_un
            |> fprintf str_formatter "%t"
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg expected actual
      in

      let r = Ast.int LocTest.dummy 1 in

      let test_not ctxt =
        let assert_op ~ctxt = function Op.Not -> () | actual -> assert_invalid_op ~ctxt Op.un_not actual in

        Ast.un_op LocTest.dummy Op.un_not r
          |> assert_un_op ~ctxt ~assert_op LocTest.dummy r
      in
      let test_neg ctxt =
        let assert_op ~ctxt = function Op.Neg -> () | actual -> assert_invalid_op ~ctxt Op.un_not actual in

        Ast.un_op LocTest.dummy Op.un_neg r
          |> assert_un_op ~ctxt ~assert_op LocTest.dummy r
      in
      "Unary Operators" >::: [
        "Boolean Negation" >:: test_not;
        "Integer Negation" >:: test_neg;
      ]
    in
    let test_bin_op =
      let assert_bin_op ~ctxt ~assert_op loc l r = function
        | Ast.BinOp (loc', l', op, r') ->
          assert_op ~ctxt op;
          LocTest.assert_loc_equal ~ctxt loc loc';
          assert_ast_equal ~ctxt l l';
          assert_ast_equal ~ctxt r r'
        | _ -> assert_failure "Invalid binary operator"
      in
      let assert_invalid_op ~ctxt expected actual =
        let cmp _ _ = false in
        let msg = "Unexpected binary operator" in
        let printer op =
          op
            |> Op.pp_bin
            |> fprintf str_formatter "%t"
            |> flush_str_formatter
        in
        assert_equal ~ctxt ~cmp ~printer ~msg expected actual
      in

      let l = Ast.int LocTest.dummy 1 in
      let r = Ast.int LocTest.dummy 2 in

      let test_add ctxt =
        let assert_op ~ctxt = function Op.Add -> () | actual -> assert_invalid_op ~ctxt Op.bin_add actual in

        Ast.bin_op LocTest.dummy l Op.bin_add r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_sub ctxt =
        let assert_op ~ctxt = function Op.Sub -> () | actual -> assert_invalid_op ~ctxt Op.bin_sub actual in

        Ast.bin_op LocTest.dummy l Op.bin_sub r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_mul ctxt =
        let assert_op ~ctxt = function Op.Mul -> () | actual -> assert_invalid_op ~ctxt Op.bin_mul actual in

        Ast.bin_op LocTest.dummy l Op.bin_mul r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_div ctxt =
        let assert_op ~ctxt = function Op.Div -> () | actual -> assert_invalid_op ~ctxt Op.bin_div actual in

        Ast.bin_op LocTest.dummy l Op.bin_div r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_mod ctxt =
        let assert_op ~ctxt = function Op.Mod -> () | actual -> assert_invalid_op ~ctxt Op.bin_mod actual in

        Ast.bin_op LocTest.dummy l Op.bin_mod r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_and ctxt =
        let assert_op ~ctxt = function Op.And -> () | actual -> assert_invalid_op ~ctxt Op.bin_and actual in

        Ast.bin_op LocTest.dummy l Op.bin_and r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_or ctxt =
        let assert_op ~ctxt = function Op.Or -> () | actual -> assert_invalid_op ~ctxt Op.bin_or actual in

        Ast.bin_op LocTest.dummy l Op.bin_or r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_eq ctxt =
        let assert_op ~ctxt = function Op.Eq -> () | actual -> assert_invalid_op ~ctxt Op.bin_eq actual in

        Ast.bin_op LocTest.dummy l Op.bin_eq r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_neq ctxt =
        let assert_op ~ctxt = function Op.Neq -> () | actual -> assert_invalid_op ~ctxt Op.bin_neq actual in

        Ast.bin_op LocTest.dummy l Op.bin_neq r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_lte ctxt =
        let assert_op ~ctxt = function Op.Lte -> () | actual -> assert_invalid_op ~ctxt Op.bin_lte actual in

        Ast.bin_op LocTest.dummy l Op.bin_lte r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_lt ctxt =
        let assert_op ~ctxt = function Op.Lt -> () | actual -> assert_invalid_op ~ctxt Op.bin_lt actual in

        Ast.bin_op LocTest.dummy l Op.bin_lt r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_gt ctxt =
        let assert_op ~ctxt = function Op.Gt -> () | actual -> assert_invalid_op ~ctxt Op.bin_gt actual in

        Ast.bin_op LocTest.dummy l Op.bin_gt r
          |> assert_bin_op ~ctxt ~assert_op LocTest.dummy l r
      in
      let test_gte ctxt =
        let assert_op ~ctxt = function Op.Gte -> () | actual -> assert_invalid_op ~ctxt Op.bin_gte actual in

        Ast.bin_op LocTest.dummy l Op.bin_gte r
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
    let test_bind _ = () in
    let test_bind_rec _ = () in
    let test_abs _ = () in
    let test_app _ = () in
    let test_var _ = () in
    let test_cond _ = () in
    "Constructors" >::: [
      "Boolean"                  >:: test_bool;
      "Integer"                  >:: test_int;
      test_un_op;
      test_bin_op;
      "Value Binding"            >:: test_bind;
      "Recursive Value Bindings" >:: test_bind_rec;
      "Function Abstraction"     >:: test_abs;
      "Function Application"     >:: test_app;
      "Variable"                 >:: test_var;
      "Conditional"              >:: test_cond;
    ]
  in
  let test_pp =
    let assert_pp ~ctxt lines ast =
      let printer str =
        let map str = sprintf "%S" str in
        str
          |> String.split_on_char '\n'
          |> List.map map
          |> String.concat "\n"
      in
      let expected = String.concat "\n" lines in
      fprintf str_formatter "%t" (Ast.pp ast)
        |> flush_str_formatter
        |> assert_equal ~ctxt ~printer expected
    in

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
    let test_un_op =
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
      "Unary Operators" >::: [
        "Equal Precedence"  >:: test_equal_precedence;
        "Higher Precedence" >:: test_higher_precedence;
        "Lower Precedence"  >:: test_lower_precedence;
      ]
    in
    let test_bin_op =
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
      "Binary Operators" >::: [
        "Constant"          >:: test_constant;
        "Equal Precedence"  >:: test_equal_precedence;
        "Higher Precedence" >:: test_higher_precedence;
        "Lower Precedence"  >:: test_lower_precedence;
        "Long"              >:: test_long;
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
               "let rec x: Int = y";
               "    and y: Int = x";
               "in";
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
               "      let temporaryVariableOne: Int = w + x in";
               "      let temporaryVariableTwo: Int = y + z in";
               "      temporaryVariableOne + temporaryVariableTwo";
               "    and finalVariable: Int =";
               "      let temporaryVariableOne: Int = w + x in";
               "      let temporaryVariableTwo: Int = y + z in";
               "      temporaryVariableOne + temporaryVariableTwo";
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
               "let rec f(w: Int, x: Int): Int = w + x in";
               "    and f(w: Int, x: Int): Int = w + x in";
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
               "      let temporaryVariableOne: Int = w + x in";
               "      let temporaryVariableTwo: Int = y + z in";
               "      temporaryVariableOne + temporaryVariableTwo";
               "    and f(w: Int, x: Int, y: Int, z: Int): Int =";
               "      let temporaryVariableOne: Int = w + x in";
               "      let temporaryVariableTwo: Int = y + z in";
               "      temporaryVariableOne + temporaryVariableTwo";
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
               "let rec f(w: Int): Int -> Int = w + x in";
               "    and f(w: Int): Int -> Int = w + x in";
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
    let test_var ctxt =
      Ast.var LocTest.dummy "variableName"
        |> assert_pp ~ctxt ["variableName"]
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
      "Boolean"                  >:: test_bool;
      "Integer"                  >:: test_int;
      test_un_op;
      test_bin_op;
      test_bind;
      test_bind_rec;
      "Function Abstraction"     >:: test_abs;
      test_app;
      "Variable"                 >:: test_var;
      test_cond;
    ]
  in
  "Abstract Syntax" >::: [
    test_constructors;
    test_pp;
  ]
