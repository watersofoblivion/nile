open Format
open OUnit2
open Nile

module type OpType =
  sig
    type t
    val pp : t -> formatter -> unit
    val equal : t -> t -> bool
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

    let equal ~ctxt op op' =
      assert_equal ~ctxt ~cmp:Op.equal op op'
    let not_equal ~ctxt op op' =
      let cmp x y = not (Op.equal x y) in
      assert_equal ~ctxt ~cmp op op'

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

module UnOp =
  struct
    type t = Op.un
    let pp = Op.pp_un
    let equal = Op.un_equal
    let precedence = Op.un_precedence
  end

module AssertUn = Assert (UnOp)

module BinOp =
  struct
    type t = Op.bin
    let pp = Op.pp_bin
    let equal = Op.bin_equal
    let precedence = Op.bin_precedence
  end

module AssertBin = Assert (BinOp)

let suite =
  let un_op_test =
    let test_constructors =
      let test_not _ =
        match Op.un_not with
          | Op.Not -> ()
          | op -> AssertUn.fail_constructor "!" op
      in
      let test_neg _ =
        match Op.un_neg with
          | Op.Neg -> ()
          | op -> AssertUn.fail_constructor "-" op
      in
      "Constructors" >::: [
        "Boolean Negation" >:: test_not;
        "Integer Negation" >:: test_neg;
      ]
    in
    let test_pp =
      let test_not ctxt = AssertUn.pp ~ctxt Op.un_not "!" in
      let test_neg ctxt = AssertUn.pp ~ctxt Op.un_neg "-" in
      "Pretty Printing" >::: [
        "Boolean Negation" >:: test_not;
        "Integer Negation" >:: test_neg;
      ]
    in
    let test_equal =
      let test_not ctxt =
        AssertUn.equal ~ctxt Op.un_not Op.un_not;
        AssertUn.not_equal ~ctxt Op.un_not Op.un_neg
      in
      let test_neg ctxt =
        AssertUn.equal ~ctxt Op.un_neg Op.un_neg;
        AssertUn.not_equal ~ctxt Op.un_neg Op.un_not
      in
      "Equality" >::: [
        "Boolean Negation" >:: test_not;
        "Integer Negation" >:: test_neg;
      ]
    in
    let test_precedence =
      let test_not ctxt = AssertUn.equal_precedence ~ctxt Op.un_not [Op.un_not; Op.un_neg] in
      let test_neg ctxt = AssertUn.equal_precedence ~ctxt Op.un_neg [Op.un_not; Op.un_neg] in
      "Precedence" >::: [
        "Boolean Negation" >:: test_not;
        "Integer Negation" >:: test_neg;
      ]
    in
    "Unary" >::: [
      test_constructors;
      test_pp;
      test_equal;
      test_precedence;
    ]
  in
  let bin_op_test =
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
      let test_or ctxt = AssertBin.pp ~ctxt Op.bin_or "||" in
      let test_eq ctxt = AssertBin.pp ~ctxt Op.bin_eq "==" in
      let test_neq ctxt = AssertBin.pp ~ctxt Op.bin_neq "!=" in
      let test_lt ctxt = AssertBin.pp ~ctxt Op.bin_lt "<" in
      let test_lte ctxt = AssertBin.pp ~ctxt Op.bin_lte "<=" in
      let test_gte ctxt = AssertBin.pp ~ctxt Op.bin_gte ">=" in
      let test_gt ctxt = AssertBin.pp ~ctxt Op.bin_gt ">" in
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
    let test_equal =
      let test_add ctxt =
        AssertBin.equal ~ctxt Op.bin_add Op.bin_add;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_add) [
          Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_sub ctxt =
        AssertBin.equal ~ctxt Op.bin_sub Op.bin_sub;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_sub) [
          Op.bin_add; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_mul ctxt =
        AssertBin.equal ~ctxt Op.bin_mul Op.bin_mul;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_mul) [
          Op.bin_add; Op.bin_sub; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_div ctxt =
        AssertBin.equal ~ctxt Op.bin_div Op.bin_div;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_div) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_mod ctxt =
        AssertBin.equal ~ctxt Op.bin_mod Op.bin_mod;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_mod) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_and ctxt =
        AssertBin.equal ~ctxt Op.bin_and Op.bin_and;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_and) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_or ctxt =
        AssertBin.equal ~ctxt Op.bin_or Op.bin_or;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_or) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_eq ctxt =
        AssertBin.equal ~ctxt Op.bin_eq Op.bin_eq;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_eq) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_neq ctxt =
        AssertBin.equal ~ctxt Op.bin_neq Op.bin_neq;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_neq) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_lte; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_lte ctxt =
        AssertBin.equal ~ctxt Op.bin_lte Op.bin_lte;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_lte) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lt; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_lt ctxt =
        AssertBin.equal ~ctxt Op.bin_lt Op.bin_lt;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_lt) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_gt; Op.bin_gte
        ]
      in
      let test_gt ctxt =
        AssertBin.equal ~ctxt Op.bin_gt Op.bin_gt;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_gt) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gte
        ]
      in
      let test_gte ctxt =
        AssertBin.equal ~ctxt Op.bin_gte Op.bin_gte;
        List.iter (AssertBin.not_equal ~ctxt Op.bin_gte) [
          Op.bin_add; Op.bin_sub; Op.bin_mul; Op.bin_div; Op.bin_mod;
          Op.bin_and; Op.bin_or;
          Op.bin_eq; Op.bin_neq; Op.bin_lte; Op.bin_lt; Op.bin_gt
        ]
      in
      "Equality" >::: [
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
        AssertBin.equal_precedence ~ctxt Op.bin_eq [Op.bin_neq];
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
        AssertBin.equal_precedence ~ctxt Op.bin_neq [Op.bin_eq];
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
      test_equal;
      test_precedence;
    ]
  in
  "Operators" >::: [
    un_op_test;
    bin_op_test;
  ]
