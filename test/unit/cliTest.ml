open Cmdliner
open OUnit2
open Nile

let suite =
  let test_args =
    let name = "unit-test" in
    let info = Term.info name in
    let empty_args = [|name|] in

    let eval_term term msg args =
      let argv = Array.append empty_args args in
      match Term.eval ~argv (term, info) with
        | `Ok actual -> actual
        | _ -> assert_failure msg
    in

    let assert_arg ~ctxt ?printer:(printer = (fun _ -> "not equal")) ?cmp:(cmp = (=)) term msg expected opts =
      eval_term term msg opts
        |> assert_equal ~ctxt ~printer ~cmp ~msg expected
    in
    let assert_flag ~ctxt term msg opts =
      let assert_flag = assert_arg ~ctxt ~printer:string_of_bool term msg in
      assert_flag false [||];
      assert_flag true opts
    in
    let assert_int_opt ~ctxt term msg expected default opts =
      let assert_int_opt = assert_arg ~ctxt ~printer:string_of_int term msg in
      assert_int_opt default [||];
      assert_int_opt expected opts
    in

    let test_opt =
      let test_tailcall ctxt =
        assert_flag ~ctxt Cli.Args.OptArgs.tailcall "Tailcall optimization" [|"--opt-tailcall"|]
      in
      let test_inline ctxt =
        assert_flag ~ctxt Cli.Args.OptArgs.inline "Inliner" [|"--opt-inline"|]
      in
      let test_ccp ctxt =
        assert_flag ~ctxt Cli.Args.OptArgs.ccp "Conditional constant propagation" [|"--opt-ccp"|]
      in
      let test_max_passes ctxt =
        assert_int_opt ~ctxt Cli.Args.OptArgs.max_passes "Maximum passes" 5 10 [|"--opt-max-passes"; "5"|]
      in
      let test_term ctxt =
        eval_term Cli.Args.OptArgs.term "Optimizer config" [||]
          |> OptTest.assert_conf ~ctxt false false false 10;
        eval_term Cli.Args.OptArgs.term "Optimizer config" [|"--opt-tailcall"|]
          |> OptTest.assert_conf ~ctxt true false false 10;
        eval_term Cli.Args.OptArgs.term "Optimizer config" [|"--opt-inline"|]
          |> OptTest.assert_conf ~ctxt false true false 10;
        eval_term Cli.Args.OptArgs.term "Optimizer config" [|"--opt-ccp"|]
          |> OptTest.assert_conf ~ctxt false false true 10;
        eval_term Cli.Args.OptArgs.term "Optimizer config" [|"--opt-max-passes"; "5"|]
          |> OptTest.assert_conf ~ctxt false false false 5
      in
      "Optimizer" >::: [
        "Tail-call Optimization"           >:: test_tailcall;
        "Inliner"                          >:: test_inline;
        "Conditional Constant Propagation" >:: test_ccp;
        "Maximum Optimizer Passes"         >:: test_max_passes;
        "Term"                             >:: test_term;
      ]
    in
    let test_clos =
      "Closure Conversion" >::: [
      ]
    in
    let test_dump =
      let assert_dump ~ctxt unannot_ast annot_ast unopt_ir opt_ir clos unopt_llvm opt_llvm actual =
        assert_equal ~ctxt ~printer:string_of_bool unannot_ast actual.Cli.Args.DumpArgs.unannot_ast ~msg:"Un-annotated AST is not correct";
        assert_equal ~ctxt ~printer:string_of_bool annot_ast actual.annot_ast ~msg:"Annotated AST is not correct";
        assert_equal ~ctxt ~printer:string_of_bool unopt_ir actual.unopt_ir ~msg:"Un-optimized IR is not correct";
        assert_equal ~ctxt ~printer:string_of_bool opt_ir actual.opt_ir ~msg:"Optimized IR is not correct";
        assert_equal ~ctxt ~printer:string_of_bool clos actual.clos ~msg:"Closure Converted is not correct";
        assert_equal ~ctxt ~printer:string_of_bool unopt_llvm actual.unopt_llvm ~msg:"Un-optimized LLVM assembly is not correct";
        assert_equal ~ctxt ~printer:string_of_bool opt_llvm actual.opt_llvm ~msg:"Optimized LLVM assembly is not correct"
      in

      let test_unannot_ast ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.unannot_ast "Un-annotated AST" [|"--dump-raw-ast"|]
      in
      let test_annot_ast ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.annot_ast "Annotated AST" [|"--dump-annot-ast"|]
      in
      let test_unopt_ir ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.unopt_ir "Un-optimized IR" [|"--dump-unopt-ir"|]
      in
      let test_opt_ir ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.unopt_ir "Optimized IR" [|"--dump-unopt-ir"|]
      in
      let test_clos ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.clos "Closure Converted" [|"--dump-clos"|]
      in
      let test_unopt_llvm ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.unopt_llvm "Un-optimized LLVM Assembly" [|"--dump-unopt-llvm"|]
      in
      let test_opt_llvm ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.opt_llvm "Optimized LLVM Assembly" [|"--dump-opt-llvm"|]
      in
      let test_all ctxt =
        assert_flag ~ctxt Cli.Args.DumpArgs.all "Dump All Forms" [|"--dump-all"|]
      in
      let test_term ctxt =
        eval_term Cli.Args.DumpArgs.term "Dump Config" [||]
          |> assert_dump ~ctxt false false false false false false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-raw-ast"|]
          |> assert_dump ~ctxt true false false false false false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-annot-ast"|]
          |> assert_dump ~ctxt false true false false false false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-unopt-ir"|]
          |> assert_dump ~ctxt false false true false false false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-opt-ir"|]
          |> assert_dump ~ctxt false false false true false false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-clos"|]
          |> assert_dump ~ctxt false false false false true false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-unopt-llvm"|]
          |> assert_dump ~ctxt false false false false false true false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-opt-llvm"|]
          |> assert_dump ~ctxt false false false false false false true;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-all"|]
          |> assert_dump ~ctxt true true true true true true true
      in
      "Dump Internal Representations" >::: [
        "Un-annotated AST"           >:: test_unannot_ast;
        "Annotated AST"              >:: test_annot_ast;
        "Un-optimized IR"            >:: test_unopt_ir;
        "Optimized IR"               >:: test_opt_ir;
        "Closure Converted"          >:: test_clos;
        "Un-optimized LLVM Assembly" >:: test_unopt_llvm;
        "Optimized LLVM Assembly"    >:: test_opt_llvm;
        "All Forms"                  >:: test_all;
        "Term"                       >:: test_term;
      ]
    in
    let test_compiler =
      "Compiler Config" >::: [
      ]
    in
    "Arguments" >::: [
      test_opt;
      test_clos;
      test_dump;
      test_compiler;
    ]
  in
  let test_cmds =
    let test_default =
      "Default" >::: [
      ]
    in
    let test_build =
      "Build" >::: [
      ]
    in
    "Commands" >::: [
      test_default;
      test_build;
    ]
  in
  "Command-Line Interface" >::: [
    test_args;
    test_cmds;
  ]
