open Format
open OUnit2
open Nile.Common

let assert_un_equal ~ctxt expected actual =
  let _ = ctxt in
  match expected, actual with
    | Op.Not, Op.Not -> ()
    (* | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Op.pp_un) ~msg:"Unary operators are not equal" expected actual *)

let assert_bin_equal ~ctxt expected actual =
  match expected, actual with
    | Op.Add, Op.Add
    | Op.Sub, Op.Sub
    | Op.Mul, Op.Mul
    | Op.Div, Op.Div
    | Op.Mod, Op.Mod
    | Op.And, Op.And
    | Op.Or, Op.Or
    | Op.Lte, Op.Lte
    | Op.Lt, Op.Lt
    | Op.Gt, Op.Gt
    | Op.Gte, Op.Gte
    | Op.Eq, Op.Eq
    | Op.Neq, Op.Neq -> ()
    | expected, actual -> assert_equal ~ctxt ~cmp:Util.never ~printer:(Util.printer Op.pp_bin) ~msg:"Binary operators are not equal" expected actual

module type OpType =
  sig
    type t
    val pp : t -> formatter -> unit
    val precedence : t -> int
  end

module Assert (Op : OpType) =
  struct
    let fail_constructor expected actual =
      fprintf str_formatter "Expected \"%s\", found \"%t\"" expected (Op.pp actual)
        |> flush_str_formatter
        |> assert_failure

    let pp ~ctxt op expected =
      fprintf str_formatter "%t" (Op.pp op)
        |> flush_str_formatter
        |> assert_equal ~ctxt expected

    let equal_precedence ~ctxt op ops =
      let prec = Op.precedence op in
      let iter op' =
        let prec' = Op.precedence op' in
        assert_equal ~ctxt prec prec'
      in
      List.iter iter ops

    let rel_precedence cmp str op ops =
      let prec = Op.precedence op in
      let iter op' =
        let prec' = Op.precedence op' in
        let msg =
          fprintf str_formatter "Expected operator \"%t\" to have %s precendence than \"%t\", but found %d and %d" (Op.pp op) str (Op.pp op') prec prec'
            |> flush_str_formatter
        in
        assert_bool msg (cmp prec prec')
      in
      List.iter iter ops
    let lower_precedence_than = rel_precedence (>) "lower"
    let higher_precedence_than = rel_precedence (<) "higher"
  end

module AssertUn = Assert (struct
  type t = Op.un
  let pp = Op.pp_un
  let precedence = Op.un_precedence
end)

module AssertBin = Assert (struct
  type t = Op.bin
  let pp = Op.pp_bin
  let precedence = Op.bin_precedence
end)

let suite =
  let test_un_op =
    let test_constructors =
      let test_not _ =
        match Op.un_not with
          | Op.Not -> ()
          (* | op -> AssertUn.fail_constructor "!" op *)
      in
      "Constructors" >::: [
        "Boolean Negation" >:: test_not;
      ]
    in
    let test_pp =
      let test_not ctxt = AssertUn.pp ~ctxt Op.un_not "!" in
      "Pretty Printing" >::: [
        "Boolean Negation" >:: test_not;
      ]
    in
    let test_precedence =
      let test_not ctxt = AssertUn.equal_precedence ~ctxt Op.un_not [Op.un_not] in
      "Precedence" >::: [
        "Boolean Negation" >:: test_not;
      ]
    in
    "Unary" >::: [
      test_constructors;
      test_pp;
      test_precedence;
    ]
  in
  let test_bin_op =
    let test_add _ =
      match Op.bin_add with
        | Op.Add -> ()
        | op -> AssertBin.fail_constructor "+" op
    in
    let test_sub _ =
      match Op.bin_sub with
        | Op.Sub -> ()
        | op -> AssertBin.fail_constructor "-" op
    in
    let test_mul _ =
      match Op.bin_mul with
        | Op.Mul -> ()
        | op -> AssertBin.fail_constructor "*" op
    in
    let test_div _ =
      match Op.bin_div with
        | Op.Div -> ()
        | op -> AssertBin.fail_constructor "/" op
    in
    let test_mod _ =
      match Op.bin_mod with
        | Op.Mod -> ()
        | op -> AssertBin.fail_constructor "%" op
    in
    let test_and _ =
      match Op.bin_and with
        | Op.And -> ()
        | op -> AssertBin.fail_constructor "&&" op
    in
    let test_or _ =
      match Op.bin_or with
        | Op.Or -> ()
        | op -> AssertBin.fail_constructor "||" op
    in
    let test_eq _ =
      match Op.bin_eq with
        | Op.Eq -> ()
        | op -> AssertBin.fail_constructor "==" op
    in
    let test_neq _ =
      match Op.bin_neq with
        | Op.Neq -> ()
        | op -> AssertBin.fail_constructor "!=" op
    in
    let test_lt _ =
      match Op.bin_lt with
        | Op.Lt -> ()
        | op -> AssertBin.fail_constructor "<" op
    in
    let test_lte _ =
      match Op.bin_lte with
        | Op.Lte -> ()
        | op -> AssertBin.fail_constructor "<=" op
    in
    let test_gte _ =
      match Op.bin_gte with
        | Op.Gte -> ()
        | op -> AssertBin.fail_constructor ">=" op
    in
    let test_gt _ =
      match Op.bin_gt with
        | Op.Gt -> ()
        | op -> AssertBin.fail_constructor ">" op
    in
    let test_constructors =
      "Constructors" >::: [
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
    let test_pp =
      let test_add ctxt = AssertBin.pp ~ctxt Op.bin_add "+" in
      let test_sub ctxt = AssertBin.pp ~ctxt Op.bin_sub "-" in
      let test_mul ctxt = AssertBin.pp ~ctxt Op.bin_mul "*" in
      let test_div ctxt = AssertBin.pp ~ctxt Op.bin_div "/" in
      let test_mod ctxt = AssertBin.pp ~ctxt Op.bin_mod "%" in
      let test_and ctxt = AssertBin.pp ~ctxt Op.bin_and "&&" in
      let test_or  ctxt = AssertBin.pp ~ctxt Op.bin_or  "||" in
      let test_eq  ctxt = AssertBin.pp ~ctxt Op.bin_eq  "==" in
      let test_neq ctxt = AssertBin.pp ~ctxt Op.bin_neq "!=" in
      let test_lt  ctxt = AssertBin.pp ~ctxt Op.bin_lt  "<" in
      let test_lte ctxt = AssertBin.pp ~ctxt Op.bin_lte "<=" in
      let test_gte ctxt = AssertBin.pp ~ctxt Op.bin_gte ">=" in
      let test_gt  ctxt = AssertBin.pp ~ctxt Op.bin_gt  ">" in
      "Pretty Printing" >::: [
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
    let test_precedence =
      let test_add ctxt =
        AssertBin.lower_precedence_than Op.bin_add [
          Op.bin_mul; Op.bin_div; Op.bin_mod
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_add [Op.bin_sub];
        AssertBin.higher_precedence_than Op.bin_add [
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_sub ctxt =
        AssertBin.lower_precedence_than Op.bin_sub [
          Op.bin_mul; Op.bin_div; Op.bin_mod
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_sub [Op.bin_add];
        AssertBin.higher_precedence_than Op.bin_sub [
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_mul ctxt =
        AssertBin.equal_precedence ~ctxt Op.bin_mul [
          Op.bin_div; Op.bin_mod;
        ];
        AssertBin.higher_precedence_than Op.bin_mul [
          Op.bin_add; Op.bin_sub;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_div ctxt =
        AssertBin.equal_precedence ~ctxt Op.bin_div [
          Op.bin_mul; Op.bin_mod;
        ];
        AssertBin.higher_precedence_than Op.bin_div [
          Op.bin_add; Op.bin_sub;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_mod ctxt =
        AssertBin.equal_precedence ~ctxt Op.bin_mod [
          Op.bin_mul; Op.bin_div;
        ];
        AssertBin.higher_precedence_than Op.bin_mod [
          Op.bin_add; Op.bin_sub;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_and _ =
        AssertBin.lower_precedence_than Op.bin_and [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
        ];
        AssertBin.higher_precedence_than Op.bin_and [
          Op.bin_or;
        ]
      in
      let test_or _ =
        AssertBin.lower_precedence_than Op.bin_or [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
        ]
      in
      let test_eq ctxt =
        AssertBin.lower_precedence_than Op.bin_eq [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_eq [Op.bin_eq; Op.bin_neq];
        AssertBin.higher_precedence_than Op.bin_eq [
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_neq ctxt =
        AssertBin.lower_precedence_than Op.bin_neq [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte;
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_neq [Op.bin_eq; Op.bin_neq];
        AssertBin.higher_precedence_than Op.bin_neq [
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_lte ctxt =
        AssertBin.lower_precedence_than Op.bin_lte [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_lte [Op.bin_lt; Op.bin_gt; Op.bin_gte];
        AssertBin.higher_precedence_than Op.bin_lte [
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_lt ctxt =
        AssertBin.lower_precedence_than Op.bin_lt [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_lt [Op.bin_lte; Op.bin_gt; Op.bin_gte];
        AssertBin.higher_precedence_than Op.bin_lt [
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_gt ctxt =
        AssertBin.lower_precedence_than Op.bin_gt [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_gt [Op.bin_lte; Op.bin_lt; Op.bin_gte];
        AssertBin.higher_precedence_than Op.bin_gt [
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      let test_gte ctxt =
        AssertBin.lower_precedence_than Op.bin_gte [
          Op.bin_add; Op.bin_sub;
          Op.bin_mul; Op.bin_div; Op.bin_mod;
        ];
        AssertBin.equal_precedence ~ctxt Op.bin_gte [Op.bin_lte; Op.bin_lt; Op.bin_gt];
        AssertBin.higher_precedence_than Op.bin_gte [
          Op.bin_eq; Op.bin_neq;
          Op.bin_and;
          Op.bin_or;
        ]
      in
      "Precedence" >::: [
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
    "Binary" >::: [
      test_constructors;
      test_pp;
      test_precedence;
    ]
  in
  "Operators" >::: [
    test_un_op;
    test_bin_op;
  ]
