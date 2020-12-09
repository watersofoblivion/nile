open Format
open OUnit2
open Nile

let suite =
  let test_constructors =
    let fail_constructor expected actual =
      fprintf str_formatter "Expected \"%s\", found \"%t\"" expected (Ast.pp actual)
        |> flush_str_formatter
        |> assert_failure
    in

    let test_bool _ =
      let assert_bool b = match Ast.bool b with
        | Ast.Bool (_, b') when b = b' -> ()
        | ast -> fail_constructor (sprintf "%b" b) ast
      in
      assert_bool true;
      assert_bool false
    in
    let test_int _ =
      let assert_int i = match Ast.int i with
        | Ast.Int (_, i') when i = i' -> ()
        | ast -> fail_constructor (sprintf "%d" i) ast
      in
      assert_int 0;
      assert_int 42;
      assert_int (-10)
    in
    let test_un_op _ =
      let assert_un_op op r =
        let expected =
          fprintf str_formatter "%t %t" (Op.pp_un op) (Ast.pp r)
            |> flush_str_formatter
        in
        match Ast.un_op op r with
          | Ast.UnOp (_, op', r') when Op.un_equal op op' && Ast.equal r r' -> ()
          | ast -> fail_constructor expected ast
      in
      assert_un_op Op.un_not (Ast.bool true);
      assert_un_op Op.un_neg (Ast.int 42)
    in
    let test_bin_op _ =
      let assert_bin_op l op r =
        let expected =
          fprintf str_formatter "%t %t %t" (Ast.pp l) (Op.pp_bin op) (Ast.pp r)
            |> flush_str_formatter
        in
        match Ast.bin_op l op r with
          | Ast.BinOp (_, l', op', r') when Ast.equal l l' && Op.bin_equal op op' && Ast.equal r r' -> ()
          | ast -> fail_constructor expected ast
      in
      assert_bin_op (Ast.int 1) Op.bin_add (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_sub (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_mul (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_div (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_mod (Ast.int 2);
      assert_bin_op (Ast.bool true) Op.bin_and (Ast.bool false);
      assert_bin_op (Ast.bool true) Op.bin_or (Ast.bool false);
      assert_bin_op (Ast.int 1) Op.bin_eq (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_neq (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_lte (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_lt (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_gt (Ast.int 2);
      assert_bin_op (Ast.int 1) Op.bin_gte (Ast.int 2)
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
      "Unary Operator"           >:: test_un_op;
      "Binary Operator"          >:: test_bin_op;
      "Value Binding"            >:: test_bind;
      "Recursive Value Bindings" >:: test_bind_rec;
      "Function Abstraction"     >:: test_abs;
      "Function Application"     >:: test_app;
      "Variable"                 >:: test_var;
      "Conditional"              >:: test_cond;
    ]
  in
  let test_pp =
    let assert_pp ~ctxt ast expected =
      let ident ast = sprintf "%S" ast in
      let expected = String.concat "\n" expected in
      fprintf str_formatter "%t" (Ast.pp ast)
        |> flush_str_formatter
        |> assert_equal ~ctxt ~printer:ident expected
    in

    let test_bool ctxt =
      assert_pp ~ctxt (Ast.bool true) ["true"];
      assert_pp ~ctxt (Ast.bool false) ["false"]
    in
    let test_int ctxt =
      assert_pp ~ctxt (Ast.int 1) ["1"];
      assert_pp ~ctxt (Ast.int 42) ["42"];
      assert_pp ~ctxt (Ast.int (-10)) ["-10"];
    in
    let test_un_op =
      let test_equal_precedence ctxt =
        let rhs = Ast.bool true in
        let rhs = Ast.un_op Op.un_not rhs in
        assert_pp ~ctxt (Ast.un_op Op.un_not rhs) ["!!true"]
      in
      let test_higher_precedence ctxt =
        let rhs = Ast.bool true in
        assert_pp ~ctxt (Ast.un_op Op.un_not rhs) ["!true"]
      in
      let test_lower_precedence ctxt =
        let rhs = Ast.bin_op (Ast.int 1) Op.bin_eq (Ast.int 2) in
        assert_pp ~ctxt (Ast.un_op Op.un_not rhs) ["!(1 == 2)"]
      in
      "Unary Operators" >::: [
        "Equal Precedence"  >:: test_equal_precedence;
        "Higher Precedence" >:: test_higher_precedence;
        "Lower Precedence"  >:: test_lower_precedence;
      ]
    in
    let test_bin_op =
      let test_constant ctxt =
        let expr = Ast.bin_op (Ast.int 1) Op.bin_add (Ast.int 2) in
        assert_pp ~ctxt expr ["1 + 2"];
      in
      let test_equal_precedence ctxt =
        let lhs = Ast.bin_op (Ast.int 1) Op.bin_add (Ast.int 2) in
        let rhs = Ast.bin_op (Ast.int 3) Op.bin_add (Ast.int 4) in

        let expr = Ast.bin_op lhs Op.bin_sub rhs in
        assert_pp ~ctxt expr ["1 + 2 - 3 + 4"];
      in
      let test_higher_precedence ctxt =
        let lhs = Ast.bin_op (Ast.int 1) Op.bin_mul (Ast.int 2) in
        let rhs = Ast.bin_op (Ast.int 3) Op.bin_mul (Ast.int 4) in
        let const = Ast.int 5 in

        let expr = Ast.bin_op lhs Op.bin_add const in
        assert_pp ~ctxt expr ["1 * 2 + 5"];

        let expr = Ast.bin_op const Op.bin_add rhs in
        assert_pp ~ctxt expr ["5 + 3 * 4"];

        let expr = Ast.bin_op lhs Op.bin_add rhs in
        assert_pp ~ctxt expr ["1 * 2 + 3 * 4"];
      in
      let test_lower_precedence ctxt =
        let lhs = Ast.bin_op (Ast.int 1) Op.bin_eq (Ast.int 2) in
        let rhs = Ast.bin_op (Ast.int 3) Op.bin_eq (Ast.int 4) in
        let const = Ast.int 5 in

        let expr = Ast.bin_op lhs Op.bin_add const in
        assert_pp ~ctxt expr ["(1 == 2) + 5"];

        let expr = Ast.bin_op const Op.bin_add rhs in
        assert_pp ~ctxt expr ["5 + (3 == 4)"];

        let expr = Ast.bin_op lhs Op.bin_add rhs in
        assert_pp ~ctxt expr ["(1 == 2) + (3 == 4)"];
      in
      let test_long ctxt =
        let name = String.make 50 'a' in
        let var = Ast.var name in
        let expr = Ast.bin_op var Op.bin_add var in
        assert_pp ~ctxt expr [
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
    let test_bind _ = () in
    let test_bind_rec _ = () in
    let test_abs _ = () in
    let test_app _ = () in
    let test_var _ = () in
    let test_cond _ = () in
    "Pretty Printing" >::: [
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
  let test_equal =
    let assert_ast_equality cmp ~ctxt ast ast' =
      let printer ast =
        fprintf str_formatter "%t" (Ast.pp ast)
          |> flush_str_formatter
      in
      assert_equal ~ctxt ~cmp ~printer ast ast'
    in
    let assert_ast_equal = assert_ast_equality Ast.equal in
    let assert_ast_not_equal =
      let cmp ast ast' = not (Ast.equal ast ast') in
      assert_ast_equality cmp
    in

    let test_bool ctxt =
      let ast = Ast.bool true in

      assert_ast_equal ~ctxt ast (Ast.bool true);
      assert_ast_not_equal ~ctxt ast (Ast.bool false);
      assert_ast_not_equal ~ctxt ast (Ast.int 42)
    in
    let test_int ctxt =
      let ast = Ast.int 42 in

      assert_ast_equal ~ctxt ast (Ast.int 42);
      assert_ast_not_equal ~ctxt ast (Ast.int 24);
      assert_ast_not_equal ~ctxt ast (Ast.bool false)
    in
    let test_un_op ctxt =
      let tru = Ast.bool true in
      let fls = Ast.bool false in
      let ast = Ast.un_op Op.un_not tru in

      assert_ast_equal ~ctxt ast (Ast.un_op Op.un_not tru);
      assert_ast_not_equal ~ctxt ast (Ast.un_op Op.un_not fls);
      assert_ast_not_equal ~ctxt ast fls
    in
    let test_bin_op ctxt =
      let one = Ast.int 1 in
      let two = Ast.int 2 in
      let fls = Ast.bool false in
      let ast = Ast.bin_op one Op.bin_add two in

      assert_ast_equal ~ctxt ast (Ast.bin_op one Op.bin_add two);
      assert_ast_not_equal ~ctxt ast (Ast.bin_op fls Op.bin_add two);
      assert_ast_not_equal ~ctxt ast (Ast.bin_op one Op.bin_add fls);
      assert_ast_not_equal ~ctxt ast one
    in
    let test_bind _ = () in
    let test_bind_rec _ = () in
    let test_abs _ = () in
    let test_app _ = () in
    let test_var _ = () in
    let test_cond _ = () in
    "Equality" >::: [
      "Boolean"                  >:: test_bool;
      "Integer"                  >:: test_int;
      "Unary Operator"           >:: test_un_op;
      "Binary Operator"          >:: test_bin_op;
      "Value Binding"            >:: test_bind;
      "Recursive Value Bindings" >:: test_bind_rec;
      "Function Abstraction"     >:: test_abs;
      "Function Application"     >:: test_app;
      "Variable"                 >:: test_var;
      "Conditional"              >:: test_cond;
    ]
  in
  "Abstract Syntax" >::: [
    test_constructors;
    test_pp;
    test_equal;
  ]
