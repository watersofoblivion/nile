open OUnit2
open Nile.Common
open Nile.Syntax

let suite =
  let test_expr =
    let test_pp =
      let assert_pp = CommonTest.Util.assert_pp Pretty.expr in

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
          let fn =
            Ast.abs LocTest.dummy id_x Type.int Type.int expr_temp_one
              |> Ast.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int)
          in
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
          let fn =
            Ast.abs LocTest.dummy id_z Type.int Type.int body
              |> Ast.abs LocTest.dummy id_y Type.int (Type.func Type.int Type.int)
              |> Ast.abs LocTest.dummy id_x Type.int (Type.func Type.int (Type.func Type.int Type.int))
              |> Ast.abs LocTest.dummy id_w Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int)))
          in
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
          let fn = Ast.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int) expr_temp_one in
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
          let fn =
            Ast.abs LocTest.dummy id_x Type.int Type.int expr_temp_one
              |> Ast.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int)
          in
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
          let fn =
            Ast.abs LocTest.dummy id_z Type.int Type.int body
              |> Ast.abs LocTest.dummy id_y Type.int (Type.func Type.int Type.int)
              |> Ast.abs LocTest.dummy id_x Type.int (Type.func Type.int (Type.func Type.int Type.int))
              |> Ast.abs LocTest.dummy id_w Type.int (Type.func Type.int (Type.func Type.int (Type.func Type.int Type.int)))
          in
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
          let fn = Ast.abs LocTest.dummy id_w Type.int (Type.func Type.int Type.int) expr_temp_one in
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

          let app_one = Ast.app LocTest.dummy var_f x in
          Ast.app LocTest.dummy app_one y
            |> assert_pp ~ctxt ["f 1 true"]
        in
        let test_literal ctxt =
          let f =
            Ast.bin_op LocTest.dummy var_x Op.bin_eq var_y
              |> Ast.abs LocTest.dummy id_y Type.int Type.bool
              |> Ast.abs LocTest.dummy id_x Type.int (Type.func Type.int Type.bool)
          in
          let x = Ast.int LocTest.dummy 1 in
          let y = Ast.bool LocTest.dummy true in

          let app_one = Ast.app LocTest.dummy f x in
          Ast.app LocTest.dummy app_one y
            |> assert_pp ~ctxt ["((x: Int, y: Int): Bool => x == y) 1 true"]
        in
        let test_func_result ctxt =
          let f = Ast.app LocTest.dummy var_f var_x in
          let x = Ast.int LocTest.dummy 1 in
          let y = Ast.bool LocTest.dummy true in

          let app_one = Ast.app LocTest.dummy f x in
          Ast.app LocTest.dummy app_one y
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
            Ast.app LocTest.dummy g z
          in

          let app_one = Ast.app LocTest.dummy var_f x in
          let app_two = Ast.app LocTest.dummy app_one y in
          Ast.app LocTest.dummy app_two z
            |> assert_pp ~ctxt ["f (1 + 2) (true && false) (g z)"];
        in
        let test_wrap ctxt =
          let f = Ast.var LocTest.dummy "aReallyLongFunctionName" in
          let x = Ast.var LocTest.dummy "aLongParameterName" in
          let y = Ast.var LocTest.dummy "yetAnotherReallyLongParameterName" in
          let z = Ast.var LocTest.dummy "whoTheHeckCameUpWithAllOfTheseReallyLongParameterNames" in

          let app_one = Ast.app LocTest.dummy f x in
          let app_two = Ast.app LocTest.dummy app_one y in
          Ast.app LocTest.dummy app_two z
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
    "Expressions" >::: [
      test_pp;
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

    let test_pp =
      let assert_pp = CommonTest.Util.assert_pp Pretty.top in

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
    "Top-Level Bindings" >::: [
      test_pp;
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

    let test_pp =
      let assert_pp = CommonTest.Util.assert_pp Pretty.file in

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
      test_pp;
    ]
  in
  "Pretty Printing" >::: [
    test_expr;
    test_top;
    test_file;
  ]
