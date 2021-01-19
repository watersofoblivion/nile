open Format
open OUnit2
open Nile.Common
open Nile.Syntax

let suite =
  let test_env =
    let id = "the-id" in
    let expected = Type.int in
    let env = Check.bind id expected Check.env in

    let test_bound ctxt =
      let actual = Check.lookup id env in
      CommonTest.TypeTest.assert_type_equal ~ctxt expected actual
    in
    let test_unbound _ =
      let id = "unbound-identifier" in
      try
        let _ = Check.lookup id env in
        sprintf "Expected %S to be unbound" id
          |> assert_failure
      with Not_found -> ()
    in
    let test_mask ctxt =
      let expected' = Type.bool in
      let env' = Check.bind id expected' env in

      let actual = Check.lookup id env in
      CommonTest.TypeTest.assert_type_equal ~ctxt expected actual;

      let actual' = Check.lookup id env' in
      CommonTest.TypeTest.assert_type_equal ~ctxt expected' actual'
    in
    "Environment" >::: [
      "Bound Identifiers"   >:: test_bound;
      "Unbound Identifiers" >:: test_unbound;
      "Masking"             >:: test_mask;
    ]
  in
  let test_type_of =
    let assert_type_of ~ctxt ?env:(env = Check.env) ty ast =
      ast
        |> Check.type_of env
        |> CommonTest.TypeTest.assert_type_equal ~ctxt ty
    in
    let assert_should_raise ~env ast =
      ast
        |> Check.type_of env
        |> Type.pp
        |> fprintf str_formatter "Expected a type error, but got type \"%t\""
        |> flush_str_formatter
        |> assert_failure
    in
    let assert_invalid_unary_operand ~ctxt ?env:(env = Check.env) expected op r ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidUnaryOperand(expected', op', r') ->
          assert_equal ~ctxt expected expected';
          assert_equal ~ctxt op op';
          assert_equal ~ctxt r r'
        | exn -> raise exn
    in
    let assert_invalid_binary_operand ~ctxt ?env:(env = Check.env) expected l op r ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidBinaryOperands(expected', l', op', r') ->
          assert_equal ~ctxt expected expected';
          assert_equal ~ctxt l l';
          assert_equal ~ctxt op op';
          assert_equal ~ctxt r r'
        | exn -> raise exn
    in
    let assert_invalid_equality_operand ~ctxt ?env:(env = Check.env) l op r ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidEqualityOperands(l', op', r') ->
          assert_equal ~ctxt l l';
          assert_equal ~ctxt op op';
          assert_equal ~ctxt r r'
        | exn -> raise exn
    in
    let assert_declaration_mismatch ~ctxt ?env:(env = Check.env) id expected actual ast =
      try assert_should_raise ~env ast
      with
        | Check.DeclarationMismatch (id', expected', actual') ->
          assert_equal ~ctxt id id';
          CommonTest.TypeTest.assert_type_equal ~ctxt expected expected';
          CommonTest.TypeTest.assert_type_equal ~ctxt actual actual'
        | exn -> raise exn
    in
    let assert_result_mismatch ~ctxt ?env:(env = Check.env) expected actual ast =
      try assert_should_raise ~env ast
      with
        | Check.ResultMismatch (expected', actual') ->
          CommonTest.TypeTest.assert_type_equal ~ctxt expected expected';
          CommonTest.TypeTest.assert_type_equal ~ctxt actual actual'
        | exn -> raise exn
    in
    let assert_unbound_identifier ~ctxt ?env:(env = Check.env) id ast =
      try assert_should_raise ~env ast
      with
        | Check.UnboundIdentifier id' ->
          assert_equal ~ctxt id id'
        | exn -> raise exn
    in
    let assert_cannot_apply ~ctxt ?env:(env = Check.env) ty ast =
      try assert_should_raise ~env ast
      with
        | Check.CannotApply ty' ->
          CommonTest.TypeTest.assert_type_equal ~ctxt ty ty'
        | exn -> raise exn
    in
    let assert_too_many_args ~ctxt ?env:(env = Check.env) ty num ast =
      try assert_should_raise ~env ast
      with
        | Check.TooManyArgs (ty', num') ->
          CommonTest.TypeTest.assert_type_equal ~ctxt ty ty';
          assert_equal ~ctxt num num'
        | exn -> raise exn
    in
    let assert_invalid_args ~ctxt ?env:(env = Check.env) expected actual ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidArgs (expected', actual') ->
          CommonTest.TypeTest.assert_type_equal ~ctxt expected expected';
          CommonTest.TypeTest.assert_type_equal ~ctxt actual actual'
        | exn -> raise exn
    in
    let assert_invalid_condition ~ctxt ?env:(env = Check.env) expected ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidCondition actual ->
          CommonTest.TypeTest.assert_type_equal ~ctxt expected actual
        | exn -> raise exn
    in
    let assert_conditional_branch_mismatch ~ctxt ?env:(env = Check.env) t f ast =
      try assert_should_raise ~env ast
      with
        | Check.ConditionalBranchMismatch (t', f') ->
          CommonTest.TypeTest.assert_type_equal ~ctxt t t';
          CommonTest.TypeTest.assert_type_equal ~ctxt f f'
        | exn -> raise exn
    in

    let i = Ast.int LocTest.dummy 1 in
    let b = Ast.bool LocTest.dummy true in

    let id = "x" in
    let x = Ast.var LocTest.dummy id in

    let test_primitive =
      let test_bool =
        let test_bool ctxt =
          b
            |> assert_type_of ~ctxt Type.bool
        in
        "Boolean" >:: test_bool
      in
      let test_int =
        let test_int ctxt =
          i
            |> assert_type_of ~ctxt Type.int
        in
        "Integer" >:: test_int
      in
      let test_var =
        let test_bound ctxt =
          let ty = Type.int in
          let env = Check.bind id ty Check.env in

          assert_type_of ~ctxt ~env ty x
        in
        let test_unbound ctxt =
          assert_unbound_identifier ~ctxt id x
        in
        "Variables" >::: [
          "Bound"   >:: test_bound;
          "Unbound" >:: test_unbound;
        ]
      in
      "Primitives" >::: [
        test_bool;
        test_int;
        test_var;
      ]
    in
    let test_operator =
      let test_un =
        let test_not =
          let test_lit ctxt =
            b
              |> Ast.un_op LocTest.dummy Op.un_not
              |> assert_type_of ~ctxt Type.bool
          in
          let test_var ctxt =
            let env = Check.bind id Type.bool Check.env in
            x
              |> Ast.un_op LocTest.dummy Op.un_not
              |> assert_type_of ~ctxt ~env Type.bool
          in
          let test_invalid_argument ctxt =
            i
              |> Ast.un_op LocTest.dummy Op.un_not
              |> assert_invalid_unary_operand ~ctxt Type.bool Op.un_not Type.int
          in
          "Boolean Negation" >::: [
            "Literal"               >:: test_lit;
            "Variable"              >:: test_var;
            "Invalid Argument Type" >:: test_invalid_argument;
          ]
        in
        "Unary" >::: [
          test_not;
        ]
      in
      let test_bin =
        let int_test op expected name =
          let test_lit ctxt =
            Ast.bin_op LocTest.dummy i op i
              |> assert_type_of ~ctxt expected
          in
          let test_invalid_argument ctxt =
            Ast.bin_op LocTest.dummy b op i
              |> assert_invalid_binary_operand ~ctxt Type.int Type.bool op Type.int;
            Ast.bin_op LocTest.dummy i op b
              |> assert_invalid_binary_operand ~ctxt Type.int Type.int op Type.bool;
            Ast.bin_op LocTest.dummy b op b
              |> assert_invalid_binary_operand ~ctxt Type.int Type.bool op Type.bool
          in
          name >::: [
            "Literal"                >:: test_lit;
            "Invalid Argument Types" >:: test_invalid_argument;
          ]
        in
        let bool_test op expected name =
          let test_lit ctxt =
            Ast.bin_op LocTest.dummy b op b
              |> assert_type_of ~ctxt expected
          in
          let test_invalid_argument ctxt =
            Ast.bin_op LocTest.dummy i op b
              |> assert_invalid_binary_operand ~ctxt Type.bool Type.int op Type.bool;
            Ast.bin_op LocTest.dummy b op i
              |> assert_invalid_binary_operand ~ctxt Type.bool Type.bool op Type.int;
            Ast.bin_op LocTest.dummy i op i
              |> assert_invalid_binary_operand ~ctxt Type.bool Type.int op Type.int
          in
          name >::: [
            "Literal"                >:: test_lit;
            "Invalid Argument Types" >:: test_invalid_argument;
          ]
        in
        let eq_test op expected name =
          let test_lit ctxt =
            Ast.bin_op LocTest.dummy b op b
              |> assert_type_of ~ctxt expected;
            Ast.bin_op LocTest.dummy i op i
              |> assert_type_of ~ctxt expected
          in
          let test_invalid_argument ctxt =
            Ast.bin_op LocTest.dummy i op b
              |> assert_invalid_equality_operand ~ctxt Type.int op Type.bool;
            Ast.bin_op LocTest.dummy b op i
              |> assert_invalid_equality_operand ~ctxt Type.bool op Type.int
          in
          name >::: [
            "Literal"                >:: test_lit;
            "Invalid Argument Types" >:: test_invalid_argument;
          ]
        in
        "Binary" >::: [
          "Addition"               |> int_test  Op.bin_add Type.int;
          "Subtraction"            |> int_test  Op.bin_sub Type.int;
          "Multiplication"         |> int_test  Op.bin_mul Type.int;
          "Integer Division"       |> int_test  Op.bin_div Type.int;
          "Modulus"                |> int_test  Op.bin_mod Type.int;
          "Logical And"            |> bool_test Op.bin_and Type.bool;
          "Logical Or"             |> bool_test Op.bin_or  Type.bool;
          "Equality"               |> eq_test   Op.bin_eq  Type.bool;
          "Inequality"             |> eq_test   Op.bin_neq Type.bool;
          "Less Than or Equal"     |> int_test  Op.bin_lte Type.bool;
          "Less Than"              |> int_test  Op.bin_lt  Type.bool;
          "Greater Than"           |> int_test  Op.bin_gt  Type.bool;
          "Greater Than or Equal"  |> int_test  Op.bin_gte Type.bool;
        ]
      in
      "Operators" >::: [
        test_un;
        test_bin;
      ]
    in
    let test_let =
      let id = "x" in
      let ty = Type.int in
      let v = Ast.int LocTest.dummy 42 in
      let b = Ast.binding LocTest.dummy id ty v in
      let rest = Ast.var LocTest.dummy id in

      let test_let ctxt =
        Ast.bind LocTest.dummy b rest
          |> assert_type_of ~ctxt ty
      in
      let test_mismatched_types ctxt =
        let v = Ast.bool LocTest.dummy true in
        let b = Ast.binding LocTest.dummy id ty v in
        let rest = Ast.var LocTest.dummy "unbound-identifier" in

        Ast.bind LocTest.dummy b rest
          |> assert_declaration_mismatch ~ctxt id ty Type.bool
      in
      "Value Bindings" >::: [
        "Success"          >:: test_let;
        "Mismatched Types" >:: test_mismatched_types;
      ]
    in
    let test_let_rec =
      let idx = "x" in
      let idy = "y" in

      let vx = Ast.var LocTest.dummy idx in
      let vy = Ast.var LocTest.dummy idy in

      let tx = Type.int in
      let ty = Type.int in

      let bx = Ast.bin_op LocTest.dummy vy Op.bin_add (Ast.int LocTest.dummy 1) in
      let by = Ast.bin_op LocTest.dummy vx Op.bin_add (Ast.int LocTest.dummy 2) in

      let bs = [
        Ast.binding LocTest.dummy idx tx bx;
        Ast.binding LocTest.dummy idy ty by;
      ] in

      let ty = Type.bool in
      let rest = Ast.bin_op LocTest.dummy vx Op.bin_eq vy in

      let test_let_rec ctxt =
        Ast.bind_rec LocTest.dummy bs rest
          |> assert_type_of ~ctxt ty
      in
      let test_incorrect_types ctxt =
        let bs = [
          Ast.binding LocTest.dummy idx Type.bool bx;
          Ast.binding LocTest.dummy idy ty by;
        ] in
        let unbound = Ast.var LocTest.dummy "unbound-identifier" in
        let unbound' = Ast.var LocTest.dummy "another-unbound-identifier" in
        let rest = Ast.bin_op LocTest.dummy unbound Op.bin_eq unbound' in

        Ast.bind_rec LocTest.dummy bs rest
          |> assert_invalid_binary_operand ~ctxt Type.int Type.bool Op.bin_add Type.int
      in
      let test_declaration_mismatch ctxt =
        let bs = [
          Ast.binding LocTest.dummy idx Type.bool bx;
          Ast.binding LocTest.dummy idy Type.int (Ast.int LocTest.dummy 1);
        ] in
        let unbound = Ast.var LocTest.dummy "unbound-identifier" in
        let unbound' = Ast.var LocTest.dummy "another-unbound-identifier" in
        let rest = Ast.bin_op LocTest.dummy unbound Op.bin_eq unbound' in

        Ast.bind_rec LocTest.dummy bs rest
          |> assert_declaration_mismatch ~ctxt idx Type.bool tx
      in
      "Recursive Value Bindings" >::: [
        "Success"              >:: test_let_rec;
        "Incorrect Types"      >:: test_incorrect_types;
        "Declaration Mismatch" >:: test_declaration_mismatch
      ]
    in
    let test_abs =
      let idx = "x" in
      let tx = Type.int in

      let idy = "y" in
      let ty = Type.int in

      let vx = Ast.var LocTest.dummy idx in
      let vy = Ast.var LocTest.dummy idy in
      let b = Ast.bin_op LocTest.dummy vx Op.bin_eq vy in
      let tb = Type.bool in

      let test_abs ctxt =
        let expected = Type.func tx (Type.func ty tb) in

        Ast.abs LocTest.dummy idy ty tb b
          |> Ast.abs LocTest.dummy idx tx (Type.func ty tb)
          |> assert_type_of ~ctxt expected
      in
      let test_result_mismatch ctxt =
        Ast.abs LocTest.dummy idy ty Type.int b
          |> Ast.abs LocTest.dummy idx tx (Type.func Type.int Type.int)
          |> assert_result_mismatch ~ctxt Type.int tb
      in
      "Function Abstraction" >::: [
        "Success"         >:: test_abs;
        "Result Mismatch" >:: test_result_mismatch;
      ]
    in
    let test_app =
      let id = "fn" in
      let fn = Ast.var LocTest.dummy id in
      let res = Type.int in
      let ty' = Type.func Type.bool res in
      let ty = Type.func Type.int ty' in
      let env = Check.bind id ty Check.env in

      let x = Ast.int LocTest.dummy 42 in
      let y = Ast.bool LocTest.dummy true in
      let z = Ast.int LocTest.dummy 0 in

      let test_app ctxt =
        let app_one = Ast.app LocTest.dummy fn x in
        Ast.app LocTest.dummy app_one y
          |> assert_type_of ~ctxt ~env Type.int
      in
      let test_partial ctxt =
        Ast.app LocTest.dummy fn x
          |> assert_type_of ~ctxt ~env ty'
      in
      let test_cannot_apply ctxt =
        let ty = Type.int in
        let env = Check.bind id ty Check.env in
        let app_one = Ast.app LocTest.dummy fn x in
        Ast.app LocTest.dummy app_one y
          |> assert_cannot_apply ~ctxt ~env ty
      in
      let test_too_many_args ctxt =
        let app_one = Ast.app LocTest.dummy fn x in
        let app_two = Ast.app LocTest.dummy app_one y in
        Ast.app LocTest.dummy app_two z
          |> assert_too_many_args ~ctxt ~env ty 3
      in
      let test_invalid_args ctxt =
        let app_one = Ast.app LocTest.dummy fn y in
        Ast.app LocTest.dummy app_one x
          |> assert_invalid_args ~ctxt ~env Type.int Type.bool;
        let app_one = Ast.app LocTest.dummy fn x in
        Ast.app LocTest.dummy app_one z
          |> assert_invalid_args ~ctxt ~env Type.bool Type.int
      in
      "Function Application" >::: [
        "Success"                >:: test_app;
        "Partial Application"    >:: test_partial;
        "Cannot Apply"           >:: test_cannot_apply;
        "Too Many Arguments"     >:: test_too_many_args;
        "Invalid Argument Types" >:: test_invalid_args;
      ]
    in
    let test_cond =
      let c = Ast.bool LocTest.dummy true in
      let t = Ast.int LocTest.dummy 1 in
      let f = Ast.int LocTest.dummy 2 in

      let test_cond ctxt =
        Ast.cond LocTest.dummy c t f
          |> assert_type_of ~ctxt Type.int
      in
      let test_invalid_condition ctxt =
        Ast.cond LocTest.dummy t t f
          |> assert_invalid_condition ~ctxt Type.int
      in
      let test_branch_mismatch ctxt =
        Ast.cond LocTest.dummy c t c
          |> assert_conditional_branch_mismatch ~ctxt Type.int Type.bool
      in
      "Conditional Expressions" >::: [
        "Success"           >:: test_cond;
        "Invalid Condition" >:: test_invalid_condition;
        "Branch Mismatch"   >:: test_branch_mismatch;
      ]
    in
    "Type Of" >::: [
      test_primitive;
      test_operator;
      test_let;
      test_let_rec;
      test_abs;
      test_app;
      test_cond;
    ]
  in
  "Type Checker" >::: [
    test_env;
    test_type_of;
  ]
