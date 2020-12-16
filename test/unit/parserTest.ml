open OUnit2
open Nile

let suite =
  let assert_parses parse ~ctxt lines expected =
    lines
      |> String.concat "\n"
      |> Lexer.from_string
      |> parse Lexer.lex
      |> AstTest.assert_ast_equal ~ctxt expected
  in
  let assert_parses_expr = assert_parses Parser.unit_test in

  let test_primitive =
    let test_bool =
      let test_true ctxt =
        let loc = Loc.mock "-" (1, 0, 0) (1, 4, 4) in
        Ast.bool loc true
          |> assert_parses_expr ~ctxt ["true"]
      in
      let test_false ctxt =
        let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
        Ast.bool loc false
          |> assert_parses_expr ~ctxt ["false"]
      in
      "Booleans" >::: [
        "True"  >:: test_true;
        "False" >:: test_false;
      ]
    in
    let test_int =
      let test_unsigned ctxt =
        let loc = Loc.mock "-" (1, 0, 0) (1, 4, 4) in
        Ast.int loc 1234
          |> assert_parses_expr ~ctxt ["1234"]
      in
      let test_positive ctxt =
        let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
        Ast.int loc 1234
          |> assert_parses_expr ~ctxt ["+1234"]
      in
      let test_negative ctxt =
        let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
        Ast.int loc (-1234)
          |> assert_parses_expr ~ctxt ["-1234"]
      in
      "Integers" >::: [
        "Unsigned" >:: test_unsigned;
        "Positive" >:: test_positive;
        "Negative" >:: test_negative;
      ]
    in
    let test_var =
      let test_var ctxt =
        let id = "x" in
        let len = String.length id in
        let loc = Loc.mock "-" (1, 0, 0) (1, len, len) in
        Ast.var loc id
          |> assert_parses_expr ~ctxt [id]
      in
      "Variables" >::: [
        "Variables" >:: test_var;
      ]
    in
    "Primitives" >::: [
      test_bool;
      test_int;
      test_var;
    ]
  in
  let test_operators =
    let test_un =
      let test_not ctxt =
        let loc_r = Loc.mock "-" (1, 1, 1) (1, 5, 5) in
        let r = Ast.bool loc_r true in

        let loc_op = Loc.mock "-" (1, 0, 0) (1, 1, 1) in
        let op = Op.un_not loc_op in

        let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
        Ast.un_op loc op r
          |> assert_parses_expr ~ctxt ["!true"]
      in
      "Unary" >::: [
        "Boolean Negation" >:: test_not;
      ]
    in
    let test_bin =
      let loc_one = Loc.mock "-" (1, 0, 0) (1, 1, 1) in
      let one = Ast.int loc_one 1 in

      let loc_two = Loc.mock "-" (1, 4, 4) (1, 5, 5) in
      let two = Ast.int loc_two 2 in

      let loc_three = Loc.mock "-" (1, 5, 5) (1, 6, 6) in
      let three = Ast.int loc_three 3 in

      let loc_true = Loc.mock "-" (1, 0, 0) (1, 4, 4) in
      let tru = Ast.bool loc_true true in

      let loc_false = Loc.mock "-" (1, 8, 8) (1, 13, 13) in
      let fls = Ast.bool loc_false false in

      let loc_op_one_ch = Loc.mock "-" (1, 2, 2) (1, 3, 3) in
      let loc_op_two_ch = Loc.mock "-" (1, 2, 2) (1, 4, 4) in
      let loc_op_log = Loc.mock "-" (1, 5, 5) (1, 7, 7) in

      let test_add ctxt =
        let op = Op.bin_add loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 + 2"]
      in
      let test_sub ctxt =
        let op = Op.bin_sub loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 - 2"]
      in
      let test_mul ctxt =
        let op = Op.bin_mul loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 * 2"]
      in
      let test_div ctxt =
        let op = Op.bin_div loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 / 2"]
      in
      let test_mod ctxt =
        let op = Op.bin_mod loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 % 2"]
      in
      let test_and ctxt =
        let op = Op.bin_and loc_op_log in
        let loc = Loc.span loc_true loc_false in
        Ast.bin_op loc tru op fls
          |> assert_parses_expr ~ctxt ["true && false"]
      in
      let test_or ctxt =
        let op = Op.bin_or loc_op_log in
        let loc = Loc.span loc_true loc_false in
        Ast.bin_op loc tru op fls
          |> assert_parses_expr ~ctxt ["true || false"]
      in
      let test_eq ctxt =
        let op = Op.bin_eq loc_op_two_ch in
        let loc = Loc.span loc_one loc_three in
        Ast.bin_op loc one op three
          |> assert_parses_expr ~ctxt ["1 == 3"]
      in
      let test_neq ctxt =
        let op = Op.bin_neq loc_op_two_ch in
        let loc = Loc.span loc_one loc_three in
        Ast.bin_op loc one op three
          |> assert_parses_expr ~ctxt ["1 != 3"]
      in
      let test_lte ctxt =
        let op = Op.bin_lte loc_op_two_ch in
        let loc = Loc.span loc_one loc_three in
        Ast.bin_op loc one op three
          |> assert_parses_expr ~ctxt ["1 <= 3"]
      in
      let test_lt ctxt =
        let op = Op.bin_lt loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 < 2"]
      in
      let test_gt ctxt =
        let op = Op.bin_gt loc_op_one_ch in
        let loc = Loc.span loc_one loc_two in
        Ast.bin_op loc one op two
          |> assert_parses_expr ~ctxt ["1 > 2"]
      in
      let test_gte ctxt =
        let op = Op.bin_gte loc_op_two_ch in
        let loc = Loc.span loc_one loc_three in
        Ast.bin_op loc one op three
          |> assert_parses_expr ~ctxt ["1 >= 3"]
      in
      "Binary" >::: [
        "Addition"              >:: test_add;
        "Subtraction"           >:: test_sub;
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
      test_un;
      test_bin;
    ]
  in
  let test_conditional =
    let test_simple ctxt =
      let loc_c = Loc.mock "-" (1, 3, 3) (1, 7, 7) in
      let loc_t = Loc.mock "-" (1, 13, 13) (1, 14, 14) in
      let loc_f = Loc.mock "-" (1, 20, 20) (1, 21, 21) in
      let c = Ast.bool loc_c true in
      let t = Ast.int loc_t 1 in
      let f = Ast.int loc_f 2 in
      let loc =
        let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
        Loc.span loc_if loc_f
      in
      Ast.cond loc c t f
        |> assert_parses_expr ~ctxt ["if true then 1 else 2"]
    in
    let test_nested_condition ctxt =
      let nested =
        let loc_c = Loc.mock "-" (2, 5, 8) (2, 9, 12) in
        let loc_t = Loc.mock "-" (2, 15, 18) (2, 20, 23) in
        let loc_f = Loc.mock "-" (2, 26, 29) (2, 30, 33) in
        let c = Ast.bool loc_c true in
        let t = Ast.bool loc_t false in
        let f = Ast.bool loc_f true in
        let loc =
          let loc_if = Loc.mock "-" (2, 2, 5) (2, 4, 7) in
          Loc.span loc_if loc_f
        in
        Ast.cond loc c t f
      in
      let loc_t = Loc.mock "-" (3, 5, 39) (3, 6, 40) in
      let loc_f = Loc.mock "-" (4, 5, 46) (4, 6, 47) in
      let loc =
        let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
        Loc.span loc_if loc_f
      in
      let t = Ast.int loc_t 1 in
      let f = Ast.int loc_f 2 in

      Ast.cond loc nested t f
        |> assert_parses_expr ~ctxt [
          "if";
          "  if true then false else true";
          "then 1";
          "else 2";
        ]
    in
    let test_nested_true ctxt =
      let nested =
        let loc_c = Loc.mock "-" (3, 5, 18) (3, 9, 22) in
        let loc_t = Loc.mock "-" (3, 15, 28) (3, 16, 29) in
        let loc_f = Loc.mock "-" (3, 22, 35) (3, 23, 36) in
        let c = Ast.bool loc_c true in
        let t = Ast.int loc_t 1 in
        let f = Ast.int loc_f 2 in
        let loc =
          let loc_if = Loc.mock "-" (3, 2, 15) (3, 4, 17) in
          Loc.span loc_if loc_f
        in
        Ast.cond loc c t f
      in
      let loc_c = Loc.mock "-" (1, 3, 3) (1, 7, 7) in
      let loc_f = Loc.mock "-" (4, 5, 42) (4, 6, 43) in
      let loc =
        let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
        Loc.span loc_if loc_f
      in
      let c = Ast.bool loc_c true in
      let f = Ast.int loc_f 3 in

      Ast.cond loc c nested f
        |> assert_parses_expr ~ctxt [
          "if true";
          "then";
          "  if true then 1 else 2";
          "else 3";
        ]
    in
    let test_nested_false ctxt =
      let (loc_nested, nested) =
        let loc_c = Loc.mock "-" (4, 5, 25) (4, 9, 29) in
        let loc_t = Loc.mock "-" (4, 15, 35) (4, 16, 36) in
        let loc_f = Loc.mock "-" (4, 22, 42) (4, 23, 43) in
        let c = Ast.bool loc_c true in
        let t = Ast.int loc_t 2 in
        let f = Ast.int loc_f 3 in
        let loc =
          let loc_if = Loc.mock "-" (4, 2, 22) (4, 4, 24) in
          Loc.span loc_if loc_f
        in
        (loc, Ast.cond loc c t f)
      in
      let loc_c = Loc.mock "-" (1, 3, 3) (1, 7, 7) in
      let loc_t = Loc.mock "-" (2, 5, 13) (2, 6, 14) in
      let loc =
        let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
        Loc.span loc_if loc_nested
      in
      let c = Ast.bool loc_c true in
      let t = Ast.int loc_t 1 in

      Ast.cond loc c t nested
        |> assert_parses_expr ~ctxt [
          "if true";
          "then 1";
          "else";
          "  if true then 2 else 3";
        ]
    in
    "Conditional" >::: [
      "Simple"              >:: test_simple;
      "Nested Condition"    >:: test_nested_condition;
      "Nested True Branch"  >:: test_nested_true;
      "Nested False Branch" >:: test_nested_false;
    ]
  in
  let test_bind =
    let test_value _ =
      ()
    in
    let test_function _ =
      ()
    in
    "Bindings" >::: [
      "Value Bindings"    >:: test_value;
      "Function Bindings" >:: test_function;
    ]
  in
  (* let test_bind =
    let test_bind _ = () in
    let test_fun_bind _ = () in
    let test_bind_rec _ = () in
    let test_fun_bind_rec _ = () in
    "Bindings" >::: [
      "Value Bindings"              >:: test_bind;
      "Function Bindings"           >:: test_fun_bind;
      "Recursive Value Bindings"    >:: test_bind_rec;
      "Recursive Function Bindings" >:: test_fun_bind_rec;
    ]
  in *)
  "Parser" >::: [
    test_primitive;
    test_operators;
    test_conditional;
    test_bind;
  ]
