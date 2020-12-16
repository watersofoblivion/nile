open Cmdliner
open OUnit2
open Nile

let suite =
  let name = "unit-test" in
  let empty_args = [|name|] in
  let info = Term.info name in

  let eval_term term msg args =
    let argv = Array.append empty_args args in
    match Term.eval ~argv (term, info) with
      | `Ok actual -> actual
      | _ -> assert_failure msg
  in
  let eval_parse_error term msg args =
    let argv = Array.append empty_args args in
    match Term.eval ~argv (term, info) with
      | `Error `Parse -> ()
      | _ -> assert_failure msg
  in
  let eval_term_error term msg args =
    let argv = Array.append empty_args args in
    match Term.eval ~argv (term, info) with
      | `Error `Term -> ()
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

  let test_args =
    let assert_dump ~ctxt unannot_ast annot_ast unopt_ir opt_ir clos unopt_llvm opt_llvm actual =
      assert_equal ~ctxt ~printer:string_of_bool unannot_ast actual.Cli.Args.DumpArgs.unannot_ast ~msg:"Un-annotated AST is not correct";
      assert_equal ~ctxt ~printer:string_of_bool annot_ast actual.annot_ast ~msg:"Annotated AST is not correct";
      assert_equal ~ctxt ~printer:string_of_bool unopt_ir actual.unopt_ir ~msg:"Un-optimized IR is not correct";
      assert_equal ~ctxt ~printer:string_of_bool opt_ir actual.opt_ir ~msg:"Optimized IR is not correct";
      assert_equal ~ctxt ~printer:string_of_bool clos actual.clos ~msg:"Closure Converted is not correct";
      assert_equal ~ctxt ~printer:string_of_bool unopt_llvm actual.unopt_llvm ~msg:"Un-optimized LLVM assembly is not correct";
      assert_equal ~ctxt ~printer:string_of_bool opt_llvm actual.opt_llvm ~msg:"Optimized LLVM assembly is not correct"
    in

    let test_opt =
      let test_args =
        let test_tailcall ctxt =
          assert_flag ~ctxt Cli.Args.OptArgs.tailcall "Tailcall optimization" [|"--opt-tailcall"|]
        in
        let test_inline ctxt =
          assert_flag ~ctxt Cli.Args.OptArgs.inline "Inliner" [|"--opt-inline"|]
        in
        let test_ccp ctxt =
          assert_flag ~ctxt Cli.Args.OptArgs.ccp "Conditional constant propagation" [|"--opt-ccp"|]
        in
        let test_max_passes =
          let test_valid ctxt =
            assert_int_opt ~ctxt Cli.Args.OptArgs.max_passes "Maximum passes" 5 10 [|"--opt-max-passes"; "5"|];
          in
          let test_invalid _ =
            eval_parse_error Cli.Args.OptArgs.term "Optimizer config" [|"--opt-max-passes"; "not-a-number"|];
          in
          let test_missing _ =
            eval_parse_error Cli.Args.OptArgs.term "Optimizer config" [|"--opt-max-passes"|]
          in
          "Maximum Optimizer Passes" >::: [
            "Valid"   >:: test_valid;
            "Invalid" >:: test_invalid;
            "Missing" >:: test_missing;
          ]
        in
        "Arguments" >::: [
          "Tail-call Optimization"           >:: test_tailcall;
          "Inliner"                          >:: test_inline;
          "Conditional Constant Propagation" >:: test_ccp;
          test_max_passes;
        ]
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
          |> OptTest.assert_conf ~ctxt false false false 5;
      in
      "Optimizer" >::: [
        test_args;
        "Term" >:: test_term;
      ]
    in
    let test_clos =
      let test_args =
        let test_mode =
          let test_default ctxt =
            assert_arg ~ctxt Cli.Args.ClosArgs.mode "Closure conversion config" Clos.Flat [||];
          in
          let test_valid ctxt =
            assert_arg ~ctxt Cli.Args.ClosArgs.mode "Closure conversion config" Clos.Flat [|"--clos-mode"; "flat"|];
            assert_arg ~ctxt Cli.Args.ClosArgs.mode "Closure conversion config" Clos.Linked [|"--clos-mode"; "linked"|];
            assert_arg ~ctxt Cli.Args.ClosArgs.mode "Closure conversion config" Clos.SSC [|"--clos-mode"; "ssc"|];
          in
          let test_invalid _ =
            eval_parse_error Cli.Args.ClosArgs.mode "Closure conversion config" [|"--clos-mode"; "invalid"|];
          in
          let test_missing _ =
            eval_parse_error Cli.Args.ClosArgs.mode "Closure conversion config" [|"--clos-mode"|]
          in
          "Mode" >::: [
            "Default" >:: test_default;
            "Valid"   >:: test_valid;
            "Invalid" >:: test_invalid;
            "Missing" >:: test_missing;
          ]
        in
        "Arguments" >::: [
          test_mode
        ]
      in
      let test_term ctxt =
        eval_term Cli.Args.ClosArgs.term "Closure conversion config" [||]
          |> ClosTest.assert_conf ~ctxt Clos.Flat;
        eval_term Cli.Args.ClosArgs.term "Closure conversion config" [|"--clos-mode"; "linked"|]
          |> ClosTest.assert_conf ~ctxt Clos.Linked
      in
      "Closure Conversion" >::: [
        test_args;
        "Term" >:: test_term;
      ]
    in
    let test_dump =
      let test_args =
        let test_unannot_ast ctxt =
          assert_flag ~ctxt Cli.Args.DumpArgs.unannot_ast "Un-annotated AST" [|"--dump-unannot-ast"|]
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
        "Arguments" >::: [
          "Un-annotated AST"           >:: test_unannot_ast;
          "Annotated AST"              >:: test_annot_ast;
          "Un-optimized IR"            >:: test_unopt_ir;
          "Optimized IR"               >:: test_opt_ir;
          "Closure Converted"          >:: test_clos;
          "Un-optimized LLVM Assembly" >:: test_unopt_llvm;
          "Optimized LLVM Assembly"    >:: test_opt_llvm;
          "All Forms"                  >:: test_all;
        ]
      in
      let test_term ctxt =
        eval_term Cli.Args.DumpArgs.term "Dump Config" [||]
          |> assert_dump ~ctxt false false false false false false false;
        eval_term Cli.Args.DumpArgs.term "Dump Config" [|"--dump-unannot-ast"|]
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
        test_args;
        "Term" >:: test_term;
      ]
    in
    let test_compiler =
      let assert_config ~ctxt opt clos dump actual =
        OptTest.assert_conf ~ctxt opt.Opt.tailcall opt.inline opt.ccp opt.max_passes actual.Cli.Args.CompilerArgs.opt;
        ClosTest.assert_conf ~ctxt clos.Clos.mode actual.clos;
        assert_dump ~ctxt dump.Cli.Args.DumpArgs.unannot_ast dump.annot_ast dump.unopt_ir dump.opt_ir dump.clos dump.unopt_llvm dump.opt_llvm actual.dump
      in

      let test_conf ctxt =
        let opt = Opt.conf true false true 7 in
        let clos = Clos.conf Clos.Linked in
        let dump = Cli.Args.DumpArgs.conf true false true false true false true false in

        Cli.Args.CompilerArgs.conf opt clos dump
          |> assert_config ~ctxt opt clos dump
      in

      let test_term ctxt =
        let opt = Opt.conf false false false 10 in
        let clos = Clos.conf Clos.Flat in
        let dump = Cli.Args.DumpArgs.conf false false false false false false false false in

        eval_term Cli.Args.CompilerArgs.term "Top-level compiler config" [||]
          |> assert_config ~ctxt opt clos dump;

        let opt = Opt.conf true false false 10 in
        let clos = Clos.conf Clos.Linked in
        let dump = Cli.Args.DumpArgs.conf true false false false false false false false in
        eval_term Cli.Args.CompilerArgs.term "Top-level compiler config" [|"--opt-tailcall"; "--clos-mode"; "linked"; "--dump-unannot-ast"|]
          |> assert_config ~ctxt opt clos dump
      in
      "Compiler Config" >::: [
        "Configration" >:: test_conf;
        "Term"         >:: test_term;
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
    let eval_cmd cmd args =
      let argv = Array.append empty_args args in
      Term.eval ~argv cmd
    in

    let test_default =
      let eval_default_cmd = eval_cmd Cli.Cmds.DefaultCmd.cmd in

      let test_cmd _ = match eval_default_cmd [||] with
        | `Help -> ()
        | _ -> assert_failure "Boom"
      in
      "Default" >::: [
        "Command" >:: test_cmd;
      ]
    in
    let test_build =
      let eval_build_cmd = eval_cmd Cli.Cmds.BuildCmd.cmd in

      let test_src =
        let test_valid ctxt =
          let (src, oc) = Filename.open_temp_file "" "" in
          let _ = close_out oc in
          assert_arg ~ctxt ~printer:Fun.id Cli.Cmds.BuildCmd.src "Input source" src [|src|]
        in
        let test_missing _ =
          eval_term_error Cli.Cmds.BuildCmd.src "Input source" [||]
        in
        let test_non_existent _ =
          eval_parse_error Cli.Cmds.BuildCmd.src "Input source" [|"doesnt-exist.nile"|]
        in
        "Input Source" >::: [
          "Valid"        >:: test_valid;
          "Missing"      >:: test_missing;
          "Non-Existent" >:: test_non_existent;
        ]
      in
      let test_exe =
        let tgt = "target.exe" in
        let printer = function
          | Some exe -> exe
          | None -> ""
        in

        let test_default ctxt =
          assert_arg ~ctxt ~printer Cli.Cmds.BuildCmd.exe "Target executable" None [||]
        in
        let test_long ctxt =
          assert_arg ~ctxt ~printer Cli.Cmds.BuildCmd.exe "Target executable" (Some tgt) [|"--output"; tgt|];
        in
        let test_short ctxt =
          assert_arg ~ctxt ~printer Cli.Cmds.BuildCmd.exe "Target executable" (Some tgt) [|"-o"; tgt|]
        in
        "Target Executable" >::: [
          "Default"      >:: test_default;
          "Long Option"  >:: test_long;
          "Short Option" >:: test_short;
        ]
      in
      let test_cmd =
        let test_valid _ =
          let (filename, oc) = Filename.open_temp_file "" "" in
          let _ = close_out oc in
          match eval_build_cmd [|"--output"; "output.exe"; filename|] with
            | `Ok _ -> ()
            | _ -> assert_failure "Boom"
        in
        let test_default_exe _ =
          let (filename, oc) = Filename.open_temp_file "" "" in
          let _ = close_out oc in
          match eval_build_cmd [|filename|] with
            | `Ok _ -> ()
            | _ -> assert_failure "Boom"
        in
        let test_missing_src _ = match eval_build_cmd [|"--output"; "output.exe"|] with
          | `Error `Term -> ()
          | _ -> assert_failure "Boom"
        in
        let test_non_existent_src _ = match eval_build_cmd [|"--output"; "output.exe"; "/non/existent/file"|] with
          | `Error `Parse -> ()
          | _ -> assert_failure "Boom"
        in
        "Command" >::: [
          "Valid"               >:: test_valid;
          "Missing Source"      >:: test_missing_src;
          "Non-Existent Source" >:: test_non_existent_src;
          "Default Target"      >:: test_default_exe;
        ]
      in
      "Build" >::: [
        test_src;
        test_exe;
        test_cmd;
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
