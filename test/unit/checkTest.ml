open Format
open OUnit2
open Nile

let suite =
  let test_env =
    let id = "the-id" in
    let expected = Type.int LocTest.dummy in
    let env = Check.bind id expected Check.env in

    let test_bound ctxt =
      let actual = Check.lookup id env in
      assert_equal ~ctxt ~cmp:Type.equal expected actual
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
      let expected' = Type.bool LocTest.dummy in
      let env' = Check.bind id expected' env in

      let actual = Check.lookup id env in
      assert_equal ~ctxt ~cmp:Type.equal expected actual;

      let actual' = Check.lookup id env' in
      assert_equal ~ctxt ~cmp:Type.equal expected' actual'
    in
    "Environment" >::: [
      "Bound Identifiers"   >:: test_bound;
      "Unbound Identifiers" >:: test_unbound;
      "Masking"             >:: test_mask;
    ]
  in
  let test_type_of =
    let assert_type_of ~ctxt ?env:(env = Check.env) expected ast =
      ast
        |> Check.type_of env
        |> assert_equal ~ctxt ~cmp:Type.equal expected
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
          assert_equal ~ctxt ~cmp:Type.equal expected expected';
          assert_equal ~ctxt ~cmp:Type.equal actual actual'
        | exn -> raise exn
    in
    let assert_result_mismatch ~ctxt ?env:(env = Check.env) expected actual ast =
      try assert_should_raise ~env ast
      with
        | Check.ResultMismatch (expected', actual') ->
          assert_equal ~ctxt ~cmp:Type.equal expected expected';
          assert_equal ~ctxt ~cmp:Type.equal actual actual'
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
          assert_equal ~ctxt ~cmp:Type.equal ty ty'
        | exn -> raise exn
    in
    let assert_too_many_args ~ctxt ?env:(env = Check.env) ty num ast =
      try assert_should_raise ~env ast
      with
        | Check.TooManyArgs (ty', num') ->
          assert_equal ~ctxt ~cmp:Type.equal ty ty';
          assert_equal ~ctxt num num'
        | exn -> raise exn
    in
    let assert_invalid_args ~ctxt ?env:(env = Check.env) expected actual ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidArgs (expected', actual') ->
          assert_equal ~ctxt ~cmp:Type.equal expected expected';
          assert_equal ~ctxt ~cmp:Type.equal actual actual'
        | exn -> raise exn
    in
    let assert_invalid_condition ~ctxt ?env:(env = Check.env) actual ast =
      try assert_should_raise ~env ast
      with
        | Check.InvalidCondition actual' ->
          assert_equal ~ctxt ~cmp:Type.equal actual actual'
        | exn -> raise exn
    in
    let assert_conditional_branch_mismatch ~ctxt ?env:(env = Check.env) t f ast =
      try assert_should_raise ~env ast
      with
        | Check.ConditionalBranchMismatch (t', f') ->
          assert_equal ~ctxt ~cmp:Type.equal t t';
          assert_equal ~ctxt ~cmp:Type.equal f f'
        | exn -> raise exn
    in

    let i = Ast.int LocTest.dummy 1 in
    let b = Ast.bool LocTest.dummy true in

    let test_primitive =
      let test_bool =
        let test_bool ctxt =
          b
            |> assert_type_of ~ctxt (Type.bool LocTest.dummy)
        in
        "Boolean" >:: test_bool
      in
      let test_int =
        let test_int ctxt =
          i
            |> assert_type_of ~ctxt (Type.int LocTest.dummy)
        in
        "Integer" >:: test_int
      in
      let test_var =
        let id = "x" in
        let v = Ast.var LocTest.dummy id in

        let test_bound ctxt =
          let ty = Type.int LocTest.dummy in
          let env = Check.bind id ty Check.env in

          assert_type_of ~ctxt ~env ty v
        in
        let test_unbound ctxt =
          assert_unbound_identifier ~ctxt id v
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
          let test_not ctxt =
            b
              |> Ast.un_op LocTest.dummy (Op.un_not LocTest.dummy)
              |> assert_type_of ~ctxt (Type.bool LocTest.dummy)
          in
          let test_invalid_argument ctxt =
            i
              |> Ast.un_op LocTest.dummy (Op.un_not LocTest.dummy)
              |> assert_invalid_unary_operand ~ctxt (Type.bool LocTest.dummy) (Op.un_not LocTest.dummy) (Type.int LocTest.dummy)
          in
          "Boolean Negation" >::: [
            "Success"               >:: test_not;
            "Invalid Argument Type" >:: test_invalid_argument;
          ]
        in
        "Unary" >::: [
          test_not;
        ]
      in
      let test_bin =
        let int_test op expected name =
          let test_op ctxt =
            Ast.bin_op LocTest.dummy i op i
              |> assert_type_of ~ctxt expected
          in
          let test_invalid_argument ctxt =
            Ast.bin_op LocTest.dummy b op i
              |> assert_invalid_binary_operand ~ctxt (Type.int LocTest.dummy) (Type.bool LocTest.dummy) op (Type.int LocTest.dummy);
            Ast.bin_op LocTest.dummy i op b
              |> assert_invalid_binary_operand ~ctxt (Type.int LocTest.dummy) (Type.int LocTest.dummy) op (Type.bool LocTest.dummy);
            Ast.bin_op LocTest.dummy b op b
              |> assert_invalid_binary_operand ~ctxt (Type.int LocTest.dummy) (Type.bool LocTest.dummy) op (Type.bool LocTest.dummy)
          in
          name >::: [
            "Success"                >:: test_op;
            "Invalid Argument Types" >:: test_invalid_argument;
          ]
        in
        let bool_test op expected name =
          let test_op ctxt =
            Ast.bin_op LocTest.dummy b op b
              |> assert_type_of ~ctxt expected
          in
          let test_invalid_argument ctxt =
            Ast.bin_op LocTest.dummy i op b
              |> assert_invalid_binary_operand ~ctxt (Type.bool LocTest.dummy) (Type.int LocTest.dummy) op (Type.bool LocTest.dummy);
            Ast.bin_op LocTest.dummy b op i
              |> assert_invalid_binary_operand ~ctxt (Type.bool LocTest.dummy) (Type.bool LocTest.dummy) op (Type.int LocTest.dummy);
            Ast.bin_op LocTest.dummy i op i
              |> assert_invalid_binary_operand ~ctxt (Type.bool LocTest.dummy) (Type.int LocTest.dummy) op (Type.int LocTest.dummy)
          in
          name >::: [
            "Success"                >:: test_op;
            "Invalid Argument Types" >:: test_invalid_argument;
          ]
        in
        let eq_test op expected name =
          let test_op ctxt =
            Ast.bin_op LocTest.dummy b op b
              |> assert_type_of ~ctxt expected;
            Ast.bin_op LocTest.dummy i op i
              |> assert_type_of ~ctxt expected
          in
          let test_invalid_argument ctxt =
            Ast.bin_op LocTest.dummy i op b
              |> assert_invalid_equality_operand ~ctxt (Type.int LocTest.dummy) op (Type.bool LocTest.dummy);
            Ast.bin_op LocTest.dummy b op i
              |> assert_invalid_equality_operand ~ctxt (Type.bool LocTest.dummy) op (Type.int LocTest.dummy)
          in
          name >::: [
            "Success"                >:: test_op;
            "Invalid Argument Types" >:: test_invalid_argument;
          ]
        in
        "Binary" >::: [
          "Addition"               |> int_test  (Op.bin_add LocTest.dummy) (Type.int LocTest.dummy);
          "Subtraction"            |> int_test  (Op.bin_sub LocTest.dummy) (Type.int LocTest.dummy);
          "Multiplication"         |> int_test  (Op.bin_mul LocTest.dummy) (Type.int LocTest.dummy);
          "Integer Division"       |> int_test  (Op.bin_div LocTest.dummy) (Type.int LocTest.dummy);
          "Modulus"                |> int_test  (Op.bin_mod LocTest.dummy) (Type.int LocTest.dummy);
          "Logical And"            |> bool_test (Op.bin_and LocTest.dummy) (Type.bool LocTest.dummy);
          "Logical Or"             |> bool_test (Op.bin_or LocTest.dummy)  (Type.bool LocTest.dummy);
          "Equality"               |> eq_test   (Op.bin_eq LocTest.dummy)  (Type.bool LocTest.dummy);
          "Inequality"             |> eq_test   (Op.bin_neq LocTest.dummy) (Type.bool LocTest.dummy);
          "Less Than or Equal"     |> int_test  (Op.bin_lte LocTest.dummy) (Type.bool LocTest.dummy);
          "Less Than"              |> int_test  (Op.bin_lt LocTest.dummy)  (Type.bool LocTest.dummy);
          "Greater Than"           |> int_test  (Op.bin_gt LocTest.dummy)  (Type.bool LocTest.dummy);
          "Greater Than or Equal"  |> int_test  (Op.bin_gte LocTest.dummy) (Type.bool LocTest.dummy);
        ]
      in
      "Operators" >::: [
        test_un;
        test_bin;
      ]
    in
    let test_let =
      let id = "x" in
      let ty = Type.int LocTest.dummy in

      let test_let ctxt =
        let v = Ast.int LocTest.dummy 42 in
        let b = Ast.binding LocTest.dummy id ty v in
        let rest = Ast.var LocTest.dummy id in

        Ast.bind LocTest.dummy b rest
          |> assert_type_of ~ctxt ty
      in
      let test_mismatched_types ctxt =
        let v = Ast.bool LocTest.dummy true in
        let b = Ast.binding LocTest.dummy id ty v in
        let rest = Ast.var LocTest.dummy "unbound-identifier" in

        Ast.bind LocTest.dummy b rest
          |> assert_declaration_mismatch ~ctxt id ty (Type.bool LocTest.dummy)
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

      let tx = Type.int LocTest.dummy in
      let ty = Type.int LocTest.dummy in

      let bx = Ast.bin_op LocTest.dummy vy (Op.bin_add LocTest.dummy) (Ast.int LocTest.dummy 1) in
      let by = Ast.bin_op LocTest.dummy vx (Op.bin_add LocTest.dummy) (Ast.int LocTest.dummy 2) in

      let bs = [
        Ast.binding LocTest.dummy idx tx bx;
        Ast.binding LocTest.dummy idy ty by;
      ] in

      let ty = Type.bool LocTest.dummy in
      let rest = Ast.bin_op LocTest.dummy vx (Op.bin_eq LocTest.dummy) vy in

      let test_let_rec ctxt =
        Ast.bind_rec LocTest.dummy bs rest
          |> assert_type_of ~ctxt ty
      in
      let test_incorrect_types ctxt =
        let bs = [
          Ast.binding LocTest.dummy idx (Type.bool LocTest.dummy) bx;
          Ast.binding LocTest.dummy idy ty by;
        ] in
        let unbound = Ast.var LocTest.dummy "unbound-identifier" in
        let unbound' = Ast.var LocTest.dummy "another-unbound-identifier" in
        let rest = Ast.bin_op LocTest.dummy unbound (Op.bin_eq LocTest.dummy) unbound' in

        Ast.bind_rec LocTest.dummy bs rest
          |> assert_invalid_binary_operand ~ctxt (Type.int LocTest.dummy) (Type.bool LocTest.dummy) (Op.bin_add LocTest.dummy) (Type.int LocTest.dummy)
      in
      let test_declaration_mismatch ctxt =
        let bs = [
          Ast.binding LocTest.dummy idx (Type.bool LocTest.dummy) bx;
          Ast.binding LocTest.dummy idy (Type.int LocTest.dummy) (Ast.int LocTest.dummy 1);
        ] in
        let unbound = Ast.var LocTest.dummy "unbound-identifier" in
        let unbound' = Ast.var LocTest.dummy "another-unbound-identifier" in
        let rest = Ast.bin_op LocTest.dummy unbound (Op.bin_eq LocTest.dummy) unbound' in

        Ast.bind_rec LocTest.dummy bs rest
          |> assert_declaration_mismatch ~ctxt idx (Type.bool LocTest.dummy) tx
      in
      "Recursive Value Bindings" >::: [
        "Success"              >:: test_let_rec;
        "Incorrect Types"      >:: test_incorrect_types;
        "Declaration Mismatch" >:: test_declaration_mismatch
      ]
    in
    let test_abs =
      let idx = "x" in
      let tx = Type.int LocTest.dummy in
      let px = Ast.param LocTest.dummy idx tx in

      let idy = "y" in
      let ty = Type.int LocTest.dummy in
      let py = Ast.param LocTest.dummy idy ty in

      let vx = Ast.var LocTest.dummy idx in
      let vy = Ast.var LocTest.dummy idy in
      let b = Ast.bin_op LocTest.dummy vx (Op.bin_eq LocTest.dummy) vy in
      let tb = Type.bool LocTest.dummy in

      let test_abs ctxt =
        let expected = Type.func LocTest.dummy tx (Type.func LocTest.dummy ty tb) in

        Ast.abs LocTest.dummy [px; py] tb b
          |> assert_type_of ~ctxt expected
      in
      let test_result_mismatch ctxt =
        Ast.abs LocTest.dummy [px; py] (Type.int LocTest.dummy) b
          |> assert_result_mismatch ~ctxt (Type.int LocTest.dummy) tb
      in
      "Function Abstraction" >::: [
        "Success"         >:: test_abs;
        "Result Mismatch" >:: test_result_mismatch;
      ]
    in
    let test_app =
      let id = "fn" in
      let fn = Ast.var LocTest.dummy id in
      let res = Type.int LocTest.dummy in
      let ty' = Type.func LocTest.dummy (Type.bool LocTest.dummy) res in
      let ty = Type.func LocTest.dummy (Type.int LocTest.dummy) ty' in
      let env = Check.bind id ty Check.env in

      let x = Ast.int LocTest.dummy 42 in
      let y = Ast.bool LocTest.dummy true in
      let z = Ast.int LocTest.dummy 0 in

      let test_app ctxt =
        Ast.app LocTest.dummy fn [x; y]
          |> assert_type_of ~ctxt ~env (Type.int LocTest.dummy)
      in
      let test_partial ctxt =
        Ast.app LocTest.dummy fn [x]
          |> assert_type_of ~ctxt ~env ty'
      in
      let test_cannot_apply ctxt =
        let ty = Type.int LocTest.dummy in
        let env = Check.bind id ty Check.env in
        Ast.app LocTest.dummy fn [x; y]
          |> assert_cannot_apply ~ctxt ~env ty
      in
      let test_too_many_args ctxt =
        Ast.app LocTest.dummy fn [x; y; z]
          |> assert_too_many_args ~ctxt ~env ty 3
      in
      let test_invalid_args ctxt =
        Ast.app LocTest.dummy fn [y; x]
          |> assert_invalid_args ~ctxt ~env (Type.int LocTest.dummy) (Type.bool LocTest.dummy);
        Ast.app LocTest.dummy fn [x; z]
          |> assert_invalid_args ~ctxt ~env (Type.bool LocTest.dummy) (Type.int LocTest.dummy)
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
          |> assert_type_of ~ctxt (Type.int LocTest.dummy)
      in
      let test_invalid_condition ctxt =
        Ast.cond LocTest.dummy t t f
          |> assert_invalid_condition ~ctxt (Type.int LocTest.dummy)
      in
      let test_branch_mismatch ctxt =
        Ast.cond LocTest.dummy c t c
          |> assert_conditional_branch_mismatch ~ctxt (Type.int LocTest.dummy) (Type.bool LocTest.dummy)
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
