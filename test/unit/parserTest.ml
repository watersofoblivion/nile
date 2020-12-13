open OUnit2

let suite =
  (* let assert_parses_top ~ctxt tops =
    let cmp = Top.equal Ast.binding_equal in
    let iter (expected, actual) = assert_equal ~ctxt ~cmp expected actual in
    let pp_sep fmt _ = fprintf fmt "@\n" in
    let pp fmt top = Top.pp Ast.pp_binding top fmt in

    tops
      |> pp_print_list ~pp_sep pp str_formatter
      |> flush_str_formatter
      |> Lexer.from_string
      |> Parser.top Lexer.lex
      |> List.map (Top.deloc Ast.deloc_binding)
      |> List.combine tops
      |> List.iter iter
  in
  let assert_parses_expr ~ctxt expr =
    [Top.bind (Ast.binding "ignored" Type.bool expr)]
      |> assert_parses_top ~ctxt
  in

  let test_primitive =
    let test_bool ctxt =
      Ast.bool true
        |> assert_parses_expr ~ctxt;
      Ast.bool false
        |> assert_parses_expr ~ctxt
    in
    let test_int ctxt =
      Ast.int 42
        |> assert_parses_expr ~ctxt;
      Ast.int 42
        |> assert_parses_expr ~ctxt;
      Ast.int (-42)
        |> assert_parses_expr ~ctxt
    in
    let test_var ctxt =
      Ast.var "foo"
        |> assert_parses_expr ~ctxt
    in
    "Primitive Values" >::: [
      "Booleans"  >:: test_bool;
      "Integers"  >:: test_int;
      "Variables" >:: test_var;
    ]
  in
  let test_operators =
    let test_add ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_add (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_sub ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_sub (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_mul ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_mul (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_div ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_div (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_mod ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_mod (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_and ctxt =
      Ast.bin_op (Ast.bool true) Op.bin_and (Ast.bool false)
        |> assert_parses_expr ~ctxt
    in
    let test_or ctxt =
      Ast.bin_op (Ast.bool true) Op.bin_or (Ast.bool false)
        |> assert_parses_expr ~ctxt
    in
    let test_not ctxt =
      Ast.un_op Op.un_not (Ast.bool false)
        |> assert_parses_expr ~ctxt
    in
    let test_eq ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_eq (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_neq ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_neq (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_lte ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_lte (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_lt ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_lt (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_gt ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_gt (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    let test_gte ctxt =
      Ast.bin_op (Ast.int 1) Op.bin_gte (Ast.int 2)
        |> assert_parses_expr ~ctxt
    in
    "Operators" >::: [
      "Addition"              >:: test_add;
      "Subtraction"           >:: test_sub;
      "Multiplication"        >:: test_mul;
      "Integer Division"      >:: test_div;
      "Modulus"               >:: test_mod;
      "Logical And"           >:: test_and;
      "Logical Or"            >:: test_or;
      "Logical Not"           >:: test_not;
      "Equality"              >:: test_eq;
      "Inequality"            >:: test_neq;
      "Less Than or Equal"    >:: test_lte;
      "Less Than"             >:: test_lt;
      "Greater Than"          >:: test_gt;
      "Greater Than or Equal" >:: test_gte;
    ]
  in
  let test_conditional =
    let test_conditional ctxt =
      Ast.cond (Ast.bool true) (Ast.int 1) (Ast.int 2)
        |> assert_parses_expr ~ctxt;
      Ast.cond (Ast.cond (Ast.bin_op (Ast.int 1) Op.bin_lt (Ast.int 2))
                         (Ast.bool true)
                         (Ast.bool false))
               (Ast.int 1)
               (Ast.int 2)
        |> assert_parses_expr ~ctxt;
      Ast.cond (Ast.bool true)
               (Ast.cond (Ast.bin_op (Ast.int 1) Op.bin_lt (Ast.int 2))
                         (Ast.int 3) (Ast.int 4))
               (Ast.int 5)
        |> assert_parses_expr ~ctxt;
      Ast.cond (Ast.bool true)
               (Ast.int 1)
               (Ast.cond (Ast.bin_op (Ast.int 2) Op.bin_lt (Ast.int 3))
                         (Ast.int 4)
                         (Ast.int 5))
        |> assert_parses_expr ~ctxt
    in
    "Conditional" >::: [
      "Conditionals" >:: test_conditional;
    ]
  in
  let test_bind =
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
    (* test_primitive;
    test_operators;
    test_conditional;
    test_bind; *)
  ]
