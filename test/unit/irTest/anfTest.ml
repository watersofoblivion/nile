open Format
open OUnit2
open Ir

let never _ _ = false
let printer pp x =
  x
    |> pp
    |> fprintf str_formatter "%t"
    |> flush_str_formatter

let assert_un_equal ~ctxt expected actual = match expected, actual with
  | Anf.Not, Anf.Not -> let _ = ctxt in ()
  (* | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_un) ~msg:"Unary operators are not equal" expected actual *)

let assert_bin_equal ~ctxt expected actual = match expected, actual with
  | Anf.Add, Anf.Add
  | Anf.Sub, Anf.Sub
  | Anf.Mul, Anf.Mul
  | Anf.Div, Anf.Div
  | Anf.Mod, Anf.Mod
  | Anf.And, Anf.And
  | Anf.Or, Anf.Or
  | Anf.Eq, Anf.Eq
  | Anf.Neq, Anf.Neq
  | Anf.Lte, Anf.Lte
  | Anf.Lt, Anf.Lt
  | Anf.Gt, Anf.Gt
  | Anf.Gte, Anf.Gte -> ()
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_bin) ~msg:"Binary operators are not equal" expected actual

let assert_prim_equal ~ctxt expected actual = match expected, actual with
  | Anf.Bool b, Anf.Bool b' -> assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" b b'
  | Anf.Int i, Anf.Int i' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer values are not equal" i i'
  | Anf.Var id, Anf.Var id' -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Identifiers are not equal" id id'
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_prim) ~msg:"Primitive values are not equal" expected actual

let assert_stmt_equal ~ctxt expected actual = match expected, actual with
  | Anf.UnOp (op, r), Anf.UnOp (op', r') ->
    assert_un_equal ~ctxt op op';
    assert_prim_equal ~ctxt r r'
  | Anf.BinOp (l, op, r), Anf.BinOp (l', op', r') ->
    assert_bin_equal ~ctxt op op';
    assert_prim_equal ~ctxt l l';
    assert_prim_equal ~ctxt r r'
  | Anf.App (fn, args), Anf.App (fn', args') ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Function names are not equal" fn fn';
    let iter (arg, arg') = assert_prim_equal ~ctxt arg arg' in
    args'
      |> List.combine args
      |> List.iter iter
  | Anf.Prim p, Anf.Prim p' -> assert_prim_equal ~ctxt p p'
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_stmt) ~msg:"Statments are not equal" expected actual

let assert_pp pp ~ctxt lines anf =
  let printer str =
    let map str = sprintf "%S" str in
    str
      |> String.split_on_char '\n'
      |> List.map map
      |> String.concat "\n"
  in
  let expected = String.concat "\n" lines in
  fprintf str_formatter "%t" (pp anf)
    |> flush_str_formatter
    |> assert_equal ~ctxt ~printer expected

let suite =
  let test_operator =
    let test_un =
      let test_constructor =
        (* let printer = printer Anf.pp_un in *)

        let test_not _ =
          let op = Anf.un_not in
          match op with
            | Anf.Not -> ()
            (* | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Unary operators are not equal" op op' *)
        in
        "Constructor" >::: [
          "Logical Negation" >:: test_not;
        ]
      in
      let test_pp =
        let assert_pp = assert_pp Anf.pp_un in

        let test_not ctxt =
          Anf.un_not
            |> assert_pp ~ctxt ["!"]
        in
        "Pretty-Printer" >::: [
          "Logical Negation" >:: test_not;
        ]
      in
      "Unary" >::: [
        test_constructor;
        test_pp;
      ]
    in
    let test_bin =
      let test_constructor =
        let printer = printer Anf.pp_bin in

        let test_add ctxt =
          let op = Anf.bin_add in
          match op with
            | Anf.Add -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_sub ctxt =
          let op = Anf.bin_sub in
          match op with
            | Anf.Sub -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_mul ctxt =
          let op = Anf.bin_mul in
          match op with
            | Anf.Mul -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_div ctxt =
          let op = Anf.bin_div in
          match op with
            | Anf.Div -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_mod ctxt =
          let op = Anf.bin_mod in
          match op with
            | Anf.Mod -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_and ctxt =
          let op = Anf.bin_and in
          match op with
            | Anf.And -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_or ctxt =
          let op = Anf.bin_or in
          match op with
            | Anf.Or -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_eq ctxt =
          let op = Anf.bin_eq in
          match op with
            | Anf.Eq -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_neq ctxt =
          let op = Anf.bin_neq in
          match op with
            | Anf.Neq -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_lte ctxt =
          let op = Anf.bin_lte in
          match op with
            | Anf.Lte -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_lt ctxt =
          let op = Anf.bin_lt in
          match op with
            | Anf.Lt -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_gt ctxt =
          let op = Anf.bin_gt in
          match op with
            | Anf.Gt -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        let test_gte ctxt =
          let op = Anf.bin_gte in
          match op with
            | Anf.Gte -> ()
            | op' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" op op'
        in
        "Constructor" >::: [
          "Addition"              >:: test_add;
          "Subtraction"           >:: test_sub;
          "Multiplication"        >:: test_mul;
          "Integer Division"      >:: test_div;
          "Modulus"               >:: test_mod;
          "Logical AND"           >:: test_and;
          "Logical OR"            >:: test_or;
          "Equality"              >:: test_eq;
          "Inequality"            >:: test_neq;
          "Less Than or Equal"    >:: test_lte;
          "Less Than"             >:: test_lt;
          "Greater Than"          >:: test_gt;
          "Greater Than or Equal" >:: test_gte;
        ]
      in
      let test_pp =
        let assert_pp = assert_pp Anf.pp_bin in

        let test_add ctxt =
          Anf.bin_add
            |> assert_pp ~ctxt ["+"]
        in
        let test_sub ctxt =
          Anf.bin_sub
            |> assert_pp ~ctxt ["-"]
        in
        let test_mul ctxt =
          Anf.bin_mul
            |> assert_pp ~ctxt ["*"]
        in
        let test_div ctxt =
          Anf.bin_div
            |> assert_pp ~ctxt ["/"]
        in
        let test_mod ctxt =
          Anf.bin_mod
            |> assert_pp ~ctxt ["%"]
        in
        let test_and ctxt =
          Anf.bin_and
            |> assert_pp ~ctxt ["&&"]
        in
        let test_or ctxt =
          Anf.bin_or
            |> assert_pp ~ctxt ["||"]
        in
        let test_eq ctxt =
          Anf.bin_eq
            |> assert_pp ~ctxt ["=="]
        in
        let test_neq ctxt =
          Anf.bin_neq
            |> assert_pp ~ctxt ["!="]
        in
        let test_lte ctxt =
          Anf.bin_lte
            |> assert_pp ~ctxt ["<="]
        in
        let test_lt ctxt =
          Anf.bin_lt
            |> assert_pp ~ctxt ["<"]
        in
        let test_gt ctxt =
          Anf.bin_gt
            |> assert_pp ~ctxt [">"]
        in
        let test_gte ctxt =
          Anf.bin_gte
            |> assert_pp ~ctxt [">="]
        in
        "Pretty-Printer" >::: [
          "Addition"              >:: test_add;
          "Subtraction"           >:: test_sub;
          "Multiplication"        >:: test_mul;
          "Integer Division"      >:: test_div;
          "Modulus"               >:: test_mod;
          "Logical AND"           >:: test_and;
          "Logical OR"            >:: test_or;
          "Equality"              >:: test_eq;
          "Inequality"            >:: test_neq;
          "Less Than or Equal"    >:: test_lte;
          "Less Than"             >:: test_lt;
          "Greater Than"          >:: test_gt;
          "Greater Than or Equal" >:: test_gte;
        ]
      in
      "Binary" >::: [
        test_constructor;
        test_pp;
      ]
    in
    "Operators" >::: [
      test_un;
      test_bin;
    ]
  in
  let test_prim =
    let test_constructor =
      let test_bool ctxt =
        let b = true in
        let anf = Anf.prim_bool b in

        match anf with
          | Anf.Bool b' -> assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" b b'
          | anf' -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_prim) ~msg:"Primitive values are not equal" anf anf'
      in
      let test_int ctxt =
        let i = 1 in
        let anf = Anf.prim_int i in

        match anf with
          | Anf.Int i' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer values are not equal" i i'
          | anf' -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_prim) ~msg:"Primitive values are not equal" anf anf'
      in
      let test_var ctxt =
        let id = "the-id" in
        let anf = Anf.prim_var id in

        match anf with
          | Anf.Var id' -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Identifiers are not equal" id id'
          | anf' -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_prim) ~msg:"Primitive values are not equal" anf anf'
      in
      "Constructor" >::: [
        "Boolean"  >:: test_bool;
        "Integer"  >:: test_int;
        "Variable" >:: test_var;
      ]
    in
    let test_pp =
      let assert_pp = assert_pp Anf.pp_prim in

      let test_bool ctxt =
        Anf.prim_bool true
          |> assert_pp ~ctxt ["true"];
        Anf.prim_bool false
          |> assert_pp ~ctxt ["false"]
      in
      let test_int ctxt =
        Anf.prim_int 42
          |> assert_pp ~ctxt ["42"];
        Anf.prim_int (-42)
          |> assert_pp ~ctxt ["-42"]
      in
      let test_var ctxt =
        let id = "the-id" in
        Anf.prim_var id
          |> assert_pp ~ctxt [id]
      in
      "Pretty-Printer" >::: [
        "Boolean"  >:: test_bool;
        "Integer"  >:: test_int;
        "Variable" >:: test_var;
      ]
    in
    "Primitives" >::: [
      test_constructor;
      test_pp;
    ]
  in
  let test_stmt =
    let test_constructor =
      let printer = printer Anf.pp_stmt in

      let test_un ctxt =
        let op = Anf.un_not in
        let r = Anf.prim_bool true in
        let stmt = Anf.stmt_un_op op r in
        match stmt with
          | Anf.UnOp (op', r') ->
            assert_un_equal ~ctxt op op';
            assert_prim_equal ~ctxt r r'
          | stmt' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Unary operators are not equal" stmt stmt'
      in
      let test_bin ctxt =
        let op = Anf.bin_add in
        let l = Anf.prim_int 1 in
        let r = Anf.prim_int 2 in
        let stmt = Anf.stmt_bin_op l op r in
        match stmt with
          | Anf.BinOp (l', op', r') ->
            assert_bin_equal ~ctxt op op';
            assert_prim_equal ~ctxt l l';
            assert_prim_equal ~ctxt r r'
          | stmt' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Binary operators are not equal" stmt stmt'
      in
      let test_app ctxt =
        let fn = "the-fn" in
        let args = [Anf.prim_int 1; Anf.prim_int 2] in
        let stmt = Anf.stmt_app fn args in
        match stmt with
          | Anf.App (fn', args') ->
            assert_equal ~ctxt ~printer:Fun.id fn fn';
            let iter (arg, arg') = assert_prim_equal ~ctxt arg arg' in
            args'
              |> List.combine args
              |> List.iter iter
          | stmt' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Applications are not equal" stmt stmt'
      in
      let test_prim ctxt =
        let p = Anf.prim_int 1 in
        let stmt = Anf.stmt_prim p in
        match stmt with
          | Anf.Prim p' -> assert_prim_equal ~ctxt p p'
          | stmt' -> assert_equal ~ctxt ~cmp:never ~printer ~msg:"Primitive statements are not equal" stmt stmt'
      in
      "Constructor" >::: [
        "Unary Operator"       >:: test_un;
        "Binary Operator"      >:: test_bin;
        "Function Application" >:: test_app;
        "Primitive"            >:: test_prim;
      ]
    in
    let test_pp =
      let assert_pp = assert_pp Anf.pp_stmt in

      let test_un ctxt =
        Anf.stmt_un_op Anf.un_not (Anf.prim_bool true)
          |> assert_pp ~ctxt ["!true"]
      in
      let test_bin ctxt =
        Anf.stmt_bin_op (Anf.prim_int 1) Anf.bin_add (Anf.prim_int 2)
          |> assert_pp ~ctxt ["1 + 2"]
      in
      let test_app ctxt =
        Anf.stmt_app "fn" [Anf.prim_int 1; Anf.prim_bool false]
          |> assert_pp ~ctxt ["fn 1 false"]
      in
      let test_prim ctxt =
        Anf.stmt_prim (Anf.prim_bool true)
          |> assert_pp ~ctxt ["true"]
      in
      "Pretty-Printer" >::: [
        "Unary Operator"       >:: test_un;
        "Binary Operator"      >:: test_bin;
        "Function Application" >:: test_app;
        "Primitive"            >:: test_prim;
      ]
    in
    "Statements" >::: [
      test_constructor;
      test_pp;
    ]
  in
  "Administrative Normal Form" >::: [
    test_operator;
    test_prim;
    test_stmt;
  ]
