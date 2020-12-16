open Format
open OUnit2
open Nile

let cmp _ _ = false

let assert_un_equal ~ctxt expected actual =
  (* let msg = "Unary operators are not equal" in
  let printer ty =
    ty
      |> Op.pp_un
      |> fprintf str_formatter "%t"
      |> flush_str_formatter
  in *)
  match expected, actual with
    | Op.Not loc, Op.Not loc' -> LocTest.assert_loc_equal ~ctxt loc loc'
    (* | expected, actual -> assert_equal ~ctxt ~cmp ~printer ~msg expected actual *)

let assert_bin_equal ~ctxt expected actual =
  let msg = "Binary operators are not equal" in
  let printer ty =
    ty
      |> Op.pp_bin
      |> fprintf str_formatter "%t"
      |> flush_str_formatter
  in
  match expected, actual with
    | Op.Add loc, Op.Add loc'
    | Op.Sub loc, Op.Sub loc'
    | Op.Mul loc, Op.Mul loc'
    | Op.Div loc, Op.Div loc'
    | Op.Mod loc, Op.Mod loc'
    | Op.And loc, Op.And loc'
    | Op.Or loc, Op.Or loc'
    | Op.Eq loc, Op.Eq loc'
    | Op.Neq loc, Op.Neq loc'
    | Op.Lte loc, Op.Lte loc'
    | Op.Lt loc, Op.Lt loc'
    | Op.Gt loc, Op.Gt loc'
    | Op.Gte loc, Op.Gte loc' -> LocTest.assert_loc_equal ~ctxt loc loc'
    | expected, actual -> assert_equal ~ctxt ~cmp ~printer ~msg expected actual

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

module AssertUn = Assert (struct
  type t = Op.un
  let pp = Op.pp_un
  let equal op op' = match op, op' with
    | Op.Not _, Op.Not _ -> true
    (* | _ -> false *)
  let precedence = Op.un_precedence
end)

module AssertBin = Assert (struct
  type t = Op.bin
  let pp = Op.pp_bin
  let equal op op' = match op, op' with
    | Op.Add _, Op.Add _ | Op.Sub _, Op.Sub _ | Op.Mul _, Op.Mul _ | Op.Div _, Op.Div _ | Op.Mod _, Op.Mod _
    | Op.And _, Op.And _ | Op.Or _, Op.Or _
    | Op.Eq _, Op.Eq _ | Op.Neq _, Op.Neq _ | Op.Lte _, Op.Lte _ | Op.Lt _, Op.Lt _ | Op.Gt _, Op.Gt _ | Op.Gte _, Op.Gte _ -> true
    | _ -> false
  let precedence = Op.bin_precedence
end)

let suite =
  let test_un_op =
    let test_constructors =
      let test_not ctxt =
        match Op.un_not LocTest.dummy with
          | Op.Not loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
          (* | op -> AssertUn.fail_constructor "!" op *)
      in
      "Constructors" >::: [
        "Boolean Negation" >:: test_not;
      ]
    in
    let test_pp =
      let test_not ctxt = AssertUn.pp ~ctxt (Op.un_not LocTest.dummy) "!" in
      "Pretty Printing" >::: [
        "Boolean Negation" >:: test_not;
      ]
    in
    let test_precedence =
      let test_not ctxt = AssertUn.equal_precedence ~ctxt (Op.un_not LocTest.dummy) [(Op.un_not LocTest.dummy)] in
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
    let test_add ctxt =
      match Op.bin_add LocTest.dummy with
        | Op.Add loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "+" op
    in
    let test_sub ctxt =
      match Op.bin_sub LocTest.dummy with
        | Op.Sub loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "-" op
    in
    let test_mul ctxt =
      match Op.bin_mul LocTest.dummy with
        | Op.Mul loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "*" op
    in
    let test_div ctxt =
      match Op.bin_div LocTest.dummy with
        | Op.Div loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "/" op
    in
    let test_mod ctxt =
      match Op.bin_mod LocTest.dummy with
        | Op.Mod loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "%" op
    in
    let test_and ctxt =
      match Op.bin_and LocTest.dummy with
        | Op.And loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "&&" op
    in
    let test_or ctxt =
      match Op.bin_or LocTest.dummy with
        | Op.Or loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "||" op
    in
    let test_eq ctxt =
      match Op.bin_eq LocTest.dummy with
        | Op.Eq loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "==" op
    in
    let test_neq ctxt =
      match Op.bin_neq LocTest.dummy with
        | Op.Neq loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "!=" op
    in
    let test_lt ctxt =
      match Op.bin_lt LocTest.dummy with
        | Op.Lt loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "<" op
    in
    let test_lte ctxt =
      match Op.bin_lte LocTest.dummy with
        | Op.Lte loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor "<=" op
    in
    let test_gte ctxt =
      match Op.bin_gte LocTest.dummy with
        | Op.Gte loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
        | op -> AssertBin.fail_constructor ">=" op
    in
    let test_gt ctxt =
      match Op.bin_gt LocTest.dummy with
        | Op.Gt loc -> LocTest.assert_loc_equal ~ctxt LocTest.dummy loc
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
      let test_add ctxt = AssertBin.pp ~ctxt (Op.bin_add LocTest.dummy) "+" in
      let test_sub ctxt = AssertBin.pp ~ctxt (Op.bin_sub LocTest.dummy) "-" in
      let test_mul ctxt = AssertBin.pp ~ctxt (Op.bin_mul LocTest.dummy) "*" in
      let test_div ctxt = AssertBin.pp ~ctxt (Op.bin_div LocTest.dummy) "/" in
      let test_mod ctxt = AssertBin.pp ~ctxt (Op.bin_mod LocTest.dummy) "%" in
      let test_and ctxt = AssertBin.pp ~ctxt (Op.bin_and LocTest.dummy) "&&" in
      let test_or ctxt = AssertBin.pp ~ctxt (Op.bin_or LocTest.dummy) "||" in
      let test_eq ctxt = AssertBin.pp ~ctxt (Op.bin_eq LocTest.dummy) "==" in
      let test_neq ctxt = AssertBin.pp ~ctxt (Op.bin_neq LocTest.dummy) "!=" in
      let test_lt ctxt = AssertBin.pp ~ctxt (Op.bin_lt LocTest.dummy) "<" in
      let test_lte ctxt = AssertBin.pp ~ctxt (Op.bin_lte LocTest.dummy) "<=" in
      let test_gte ctxt = AssertBin.pp ~ctxt (Op.bin_gte LocTest.dummy) ">=" in
      let test_gt ctxt = AssertBin.pp ~ctxt (Op.bin_gt LocTest.dummy) ">" in
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
        AssertBin.lower_precedence_than (Op.bin_add LocTest.dummy) [
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy)
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_add LocTest.dummy) [(Op.bin_sub LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_add LocTest.dummy) [
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_sub ctxt =
        AssertBin.lower_precedence_than (Op.bin_sub LocTest.dummy) [
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy)
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_sub LocTest.dummy) [(Op.bin_add LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_sub LocTest.dummy) [
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_mul ctxt =
        AssertBin.equal_precedence ~ctxt (Op.bin_mul LocTest.dummy) [
          (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
        ];
        AssertBin.higher_precedence_than (Op.bin_mul LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_div ctxt =
        AssertBin.equal_precedence ~ctxt (Op.bin_div LocTest.dummy) [
          (Op.bin_mul LocTest.dummy); (Op.bin_mod LocTest.dummy);
        ];
        AssertBin.higher_precedence_than (Op.bin_div LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_mod ctxt =
        AssertBin.equal_precedence ~ctxt (Op.bin_mod LocTest.dummy) [
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy);
        ];
        AssertBin.higher_precedence_than (Op.bin_mod LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_and _ =
        AssertBin.lower_precedence_than (Op.bin_and LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
        ];
        AssertBin.higher_precedence_than (Op.bin_and LocTest.dummy) [
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_or _ =
        AssertBin.lower_precedence_than (Op.bin_or LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
        ]
      in
      let test_eq ctxt =
        AssertBin.lower_precedence_than (Op.bin_eq LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_eq LocTest.dummy) [(Op.bin_neq LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_eq LocTest.dummy) [
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_neq ctxt =
        AssertBin.lower_precedence_than (Op.bin_neq LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
          (Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy);
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_neq LocTest.dummy) [(Op.bin_eq LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_neq LocTest.dummy) [
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_lte ctxt =
        AssertBin.lower_precedence_than (Op.bin_lte LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_lte LocTest.dummy) [(Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_lte LocTest.dummy) [
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_lt ctxt =
        AssertBin.lower_precedence_than (Op.bin_lt LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_lt LocTest.dummy) [(Op.bin_lte LocTest.dummy); (Op.bin_gt LocTest.dummy); (Op.bin_gte LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_lt LocTest.dummy) [
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_gt ctxt =
        AssertBin.lower_precedence_than (Op.bin_gt LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_gt LocTest.dummy) [(Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gte LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_gt LocTest.dummy) [
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
        ]
      in
      let test_gte ctxt =
        AssertBin.lower_precedence_than (Op.bin_gte LocTest.dummy) [
          (Op.bin_add LocTest.dummy); (Op.bin_sub LocTest.dummy);
          (Op.bin_mul LocTest.dummy); (Op.bin_div LocTest.dummy); (Op.bin_mod LocTest.dummy);
        ];
        AssertBin.equal_precedence ~ctxt (Op.bin_gte LocTest.dummy) [(Op.bin_lte LocTest.dummy); (Op.bin_lt LocTest.dummy); (Op.bin_gt LocTest.dummy)];
        AssertBin.higher_precedence_than (Op.bin_gte LocTest.dummy) [
          (Op.bin_eq LocTest.dummy); (Op.bin_neq LocTest.dummy);
          (Op.bin_and LocTest.dummy);
          (Op.bin_or LocTest.dummy);
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
