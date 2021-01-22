open OUnit2
open Nile.Common
open Nile.Syntax

let assert_parses parse ~ctxt lines expected =
  lines
    |> String.concat "\n"
    |> Lexer.from_string
    |> parse Lexer.lex
    |> UnannotTest.assert_expr_equal ~ctxt expected

let assert_parses_expr = assert_parses Parser.unit_test

let test_bool_true ctxt =
  let loc = Loc.mock "-" (1, 0, 0) (1, 4, 4) in
  Unannot.bool loc true
    |> assert_parses_expr ~ctxt ["true"]

let test_bool_false ctxt =
  let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
  Unannot.bool loc false
    |> assert_parses_expr ~ctxt ["false"]

let test_int_unsigned ctxt =
  let loc = Loc.mock "-" (1, 0, 0) (1, 4, 4) in
  Unannot.int loc 1234
    |> assert_parses_expr ~ctxt ["1234"]

let test_int_positive ctxt =
  let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
  Unannot.int loc 1234
    |> assert_parses_expr ~ctxt ["+1234"]

let test_int_negative ctxt =
  let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
  Unannot.int loc (-1234)
    |> assert_parses_expr ~ctxt ["-1234"]

let test_var ctxt =
  let id = "x" in
  let len = String.length id in
  let loc = Loc.mock "-" (1, 0, 0) (1, len, len) in
  Unannot.var loc id
    |> assert_parses_expr ~ctxt [id]

let test_un_op_not ctxt =
  let loc_r = Loc.mock "-" (1, 1, 1) (1, 5, 5) in
  let r = Unannot.bool loc_r true in

  let op = Op.un_not in

  let loc = Loc.mock "-" (1, 0, 0) (1, 5, 5) in
  Unannot.un_op loc op r
    |> assert_parses_expr ~ctxt ["!true"]

let loc_one = Loc.mock "-" (1, 0, 0) (1, 1, 1)
let one = Unannot.int loc_one 1

let loc_two = Loc.mock "-" (1, 4, 4) (1, 5, 5)
let two = Unannot.int loc_two 2

let loc_three = Loc.mock "-" (1, 5, 5) (1, 6, 6)
let three = Unannot.int loc_three 3

let loc_true = Loc.mock "-" (1, 0, 0) (1, 4, 4)
let tru = Unannot.bool loc_true true

let loc_false = Loc.mock "-" (1, 8, 8) (1, 13, 13)
let fls = Unannot.bool loc_false false

let test_bin_op_add ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_add two
    |> assert_parses_expr ~ctxt ["1 + 2"]

let test_bin_op_sub ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_sub two
    |> assert_parses_expr ~ctxt ["1 - 2"]

let test_bin_op_mul ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_mul two
    |> assert_parses_expr ~ctxt ["1 * 2"]

let test_bin_op_div ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_div two
    |> assert_parses_expr ~ctxt ["1 / 2"]

let test_bin_op_mod ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_mod two
    |> assert_parses_expr ~ctxt ["1 % 2"]

let test_bin_op_and ctxt =
  let loc = Loc.span loc_true loc_false in
  Unannot.bin_op loc tru Op.bin_and fls
    |> assert_parses_expr ~ctxt ["true && false"]

let test_bin_op_or ctxt =
  let loc = Loc.span loc_true loc_false in
  Unannot.bin_op loc tru Op.bin_or fls
    |> assert_parses_expr ~ctxt ["true || false"]

let test_bin_op_eq ctxt =
  let loc = Loc.span loc_one loc_three in
  Unannot.bin_op loc one Op.bin_eq three
    |> assert_parses_expr ~ctxt ["1 == 3"]

let test_bin_op_neq ctxt =
  let loc = Loc.span loc_one loc_three in
  Unannot.bin_op loc one Op.bin_neq three
    |> assert_parses_expr ~ctxt ["1 != 3"]

let test_bin_op_lte ctxt =
  let loc = Loc.span loc_one loc_three in
  Unannot.bin_op loc one Op.bin_lte three
    |> assert_parses_expr ~ctxt ["1 <= 3"]

let test_bin_op_lt ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_lt two
    |> assert_parses_expr ~ctxt ["1 < 2"]

let test_bin_op_gt ctxt =
  let loc = Loc.span loc_one loc_two in
  Unannot.bin_op loc one Op.bin_gt two
    |> assert_parses_expr ~ctxt ["1 > 2"]

let test_bin_op_gte ctxt =
  let loc = Loc.span loc_one loc_three in
  Unannot.bin_op loc one Op.bin_gte three
    |> assert_parses_expr ~ctxt ["1 >= 3"]

let test_cond_simple ctxt =
  let loc_c = Loc.mock "-" (1, 3, 3) (1, 7, 7) in
  let loc_t = Loc.mock "-" (1, 13, 13) (1, 14, 14) in
  let loc_f = Loc.mock "-" (1, 20, 20) (1, 21, 21) in
  let c = Unannot.bool loc_c true in
  let t = Unannot.int loc_t 1 in
  let f = Unannot.int loc_f 2 in
  let loc =
    let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
    Loc.span loc_if loc_f
  in
  Unannot.cond loc c t f
    |> assert_parses_expr ~ctxt ["if true then 1 else 2"]

let test_cond_nested_condition ctxt =
  let nested =
    let loc_c = Loc.mock "-" (2, 5, 8) (2, 9, 12) in
    let loc_t = Loc.mock "-" (2, 15, 18) (2, 20, 23) in
    let loc_f = Loc.mock "-" (2, 26, 29) (2, 30, 33) in
    let c = Unannot.bool loc_c true in
    let t = Unannot.bool loc_t false in
    let f = Unannot.bool loc_f true in
    let loc =
      let loc_if = Loc.mock "-" (2, 2, 5) (2, 4, 7) in
      Loc.span loc_if loc_f
    in
    Unannot.cond loc c t f
  in
  let loc_t = Loc.mock "-" (3, 5, 39) (3, 6, 40) in
  let loc_f = Loc.mock "-" (4, 5, 46) (4, 6, 47) in
  let loc =
    let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
    Loc.span loc_if loc_f
  in
  let t = Unannot.int loc_t 1 in
  let f = Unannot.int loc_f 2 in

  Unannot.cond loc nested t f
    |> assert_parses_expr ~ctxt [
      "if";
      "  if true then false else true";
      "then 1";
      "else 2";
    ]

let test_cond_nested_true ctxt =
  let nested =
    let loc_c = Loc.mock "-" (3, 5, 18) (3, 9, 22) in
    let loc_t = Loc.mock "-" (3, 15, 28) (3, 16, 29) in
    let loc_f = Loc.mock "-" (3, 22, 35) (3, 23, 36) in
    let c = Unannot.bool loc_c true in
    let t = Unannot.int loc_t 1 in
    let f = Unannot.int loc_f 2 in
    let loc =
      let loc_if = Loc.mock "-" (3, 2, 15) (3, 4, 17) in
      Loc.span loc_if loc_f
    in
    Unannot.cond loc c t f
  in
  let loc_c = Loc.mock "-" (1, 3, 3) (1, 7, 7) in
  let loc_f = Loc.mock "-" (4, 5, 42) (4, 6, 43) in
  let loc =
    let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
    Loc.span loc_if loc_f
  in
  let c = Unannot.bool loc_c true in
  let f = Unannot.int loc_f 3 in

  Unannot.cond loc c nested f
    |> assert_parses_expr ~ctxt [
      "if true";
      "then";
      "  if true then 1 else 2";
      "else 3";
    ]

let test_cond_nested_false ctxt =
  let (loc_nested, nested) =
    let loc_c = Loc.mock "-" (4, 5, 25) (4, 9, 29) in
    let loc_t = Loc.mock "-" (4, 15, 35) (4, 16, 36) in
    let loc_f = Loc.mock "-" (4, 22, 42) (4, 23, 43) in
    let c = Unannot.bool loc_c true in
    let t = Unannot.int loc_t 2 in
    let f = Unannot.int loc_f 3 in
    let loc =
      let loc_if = Loc.mock "-" (4, 2, 22) (4, 4, 24) in
      Loc.span loc_if loc_f
    in
    (loc, Unannot.cond loc c t f)
  in
  let loc_c = Loc.mock "-" (1, 3, 3) (1, 7, 7) in
  let loc_t = Loc.mock "-" (2, 5, 13) (2, 6, 14) in
  let loc =
    let loc_if = Loc.mock "-" (1, 0, 0) (1, 2, 2) in
    Loc.span loc_if loc_nested
  in
  let c = Unannot.bool loc_c true in
  let t = Unannot.int loc_t 1 in

  Unannot.cond loc c t nested
    |> assert_parses_expr ~ctxt [
      "if true";
      "then 1";
      "else";
      "  if true then 2 else 3";
    ]

(* Suite *)

let suite =
  "Parser" >::: [
    "Expressions" >::: [
      "Primitives" >::: [
        "Boolean" >::: [
          "True"  >:: test_bool_true;
          "False" >:: test_bool_false;
        ];
        "Integer" >::: [
          "Unsigned" >:: test_int_unsigned;
          "Positive" >:: test_int_positive;
          "Negative" >:: test_int_negative;
        ];
        "Variable" >:: test_var;
      ];
      "Operators" >::: [
        "Unary" >::: [
          "Boolean Not" >:: test_un_op_not;
        ];
        "Binary" >::: [
          "Addition"              >:: test_bin_op_add;
          "Subtraction"           >:: test_bin_op_sub;
          "Multiplication"        >:: test_bin_op_mul;
          "Integer Division"      >:: test_bin_op_div;
          "Modulus"               >:: test_bin_op_mod;
          "Logical And"           >:: test_bin_op_and;
          "Logical Or"            >:: test_bin_op_or;
          "Equality"              >:: test_bin_op_eq;
          "Inequality"            >:: test_bin_op_neq;
          "Less Than or Equal"    >:: test_bin_op_lte;
          "Less Than"             >:: test_bin_op_lt;
          "Greater Than"          >:: test_bin_op_gt;
          "Greater Than or Equal" >:: test_bin_op_gte;
        ];
      ];
      "Conditional" >::: [
        "Simple"              >:: test_cond_simple;
        "Nested Condition"    >:: test_cond_nested_condition;
        "Nested True Branch"  >:: test_cond_nested_true;
        "Nested False Branch" >:: test_cond_nested_false;
      ];
    ];
  ]
