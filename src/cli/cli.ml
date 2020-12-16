open Format
open Cmdliner

(* Optimizer *)

module Args =
  struct
    module OptArgs =
      struct
        let docs = "OPTIONS: OPTIMIZER"

        let tailcall =
          let doc = "Enable tail-call optimization" in
          Arg.(value & flag & info ["opt-tailcall"] ~doc ~docs)

        let inline =
          let doc = "Enable inliner" in
          Arg.(value & flag & info ["opt-inline"] ~doc ~docs)

        let ccp =
          let doc = "Enable conditional constant propagation" in
          Arg.(value & flag & info ["opt-ccp"] ~doc ~docs)

        let max_passes =
          let default = 10 in
          let doc = "Maximum number of optimization passes" in
          let docv = "MAXIMUM" in
          Arg.(value & opt int default & info ["opt-max-passes"] ~doc ~docv ~docs)

        let term = Term.(const Ir.Opt.conf $ tailcall $ inline $ ccp $ max_passes)
      end

    (* Closure Conversion *)

    module ClosArgs =
      struct
        let docs = "OPTIONS: CLOSURE CONVERSION MODE"

        let mode =
          let default =
            Codegen.Clos.modes
              |> List.hd
              |> snd
          in
          let doc =
            Codegen.Clos.modes
              |> List.map fst
              |> String.concat ", "
              |> sprintf "Select the closure conversion mode.  Valid values are: %s"
          in
          let docv = "MODE" in
          Arg.(value & opt (enum Codegen.Clos.modes) default & info ["clos-mode"] ~doc ~docv ~docs)

        let term = Term.(const Codegen.Clos.conf $ mode)
      end

    (* Dumping *)

    module DumpArgs =
      struct
        type conf = {
          unannot_ast : bool;
          annot_ast   : bool;
          unopt_ir    : bool;
          opt_ir      : bool;
          clos        : bool;
          unopt_llvm  : bool;
          opt_llvm    : bool;
        }

        let conf unannot_ast annot_ast unopt_ir opt_ir clos unopt_llvm opt_llvm all =
          if all
          then
            { unannot_ast = true;
              annot_ast   = true;
              unopt_ir    = true;
              opt_ir      = true;
              clos        = true;
              unopt_llvm  = true;
              opt_llvm    = true; }
          else
            { unannot_ast = unannot_ast;
              annot_ast   = annot_ast;
              unopt_ir    = unopt_ir;
              opt_ir      = opt_ir;
              clos        = clos;
              unopt_llvm  = unopt_llvm;
              opt_llvm    = opt_llvm; }

        let docs = "OPTIONS: DUMP INTERNAL REPRESENTATIONS"

        let unannot_ast =
          let doc = "Print the un-annotated abstract syntax tree to STDERR" in
          Arg.(value & flag & info ["dump-unannot-ast"] ~doc ~docs)

        let annot_ast =
          let doc = "Print the annotated abstract syntax tree to STDERR" in
          Arg.(value & flag & info ["dump-annot-ast"] ~doc ~docs)

        let unopt_ir =
          let doc = "Print the unoptimized intermediate representation (ANF) to STDERR" in
          Arg.(value & flag & info ["dump-unopt-ir"] ~doc ~docs)

        let opt_ir =
          let doc = "Print the optimized intermediate representation (ANF) to STDERR" in
          Arg.(value & flag & info ["dump-opt-ir"] ~doc ~docs)

        let clos =
          let doc = "Print the closure-converted program to STDERR" in
          Arg.(value & flag & info ["dump-clos"] ~doc ~docs)

        let unopt_llvm =
          let doc = "Print the unoptimized LLVM assembly to STDERR" in
          Arg.(value & flag & info ["dump-unopt-llvm"] ~doc ~docs)

        let opt_llvm =
          let doc = "Print the unoptimized LLVM assembly to STDERR" in
          Arg.(value & flag & info ["dump-opt-llvm"] ~doc ~docs)

        let all =
          let doc = "Print all of the intermediate forms to STDERR" in
          Arg.(value & flag & info ["dump-all"] ~doc ~docs)

        let term = Term.(const conf $ unannot_ast $ annot_ast $ unopt_ir $ opt_ir $ clos $ unopt_llvm $ opt_llvm $ all)
      end

    (* Top-level configuration *)

    module CompilerArgs =
      struct
        type conf = {
          opt  : Ir.Opt.conf;
          clos : Codegen.Clos.conf;
          dump : DumpArgs.conf;
        }

        let conf opt clos dump =
          { opt  = opt;
            clos = clos;
            dump = dump; }

        let term = Term.(const conf $ OptArgs.term $ ClosArgs.term $ DumpArgs.term)
      end
  end

(* Commands *)

module Cmds =
  struct
    module DefaultCmd =
      struct
        let doc = "An example compiler"
        let sdocs = Manpage.s_common_options
        let man = [
          `S Manpage.s_description;

          `P ("$(mname) is an example compiler.")
        ]

        let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ()))
        let info = Term.info "nile" ~doc ~sdocs ~exits:Term.default_exits ~man

        let cmd = (term, info)
      end

    module BuildCmd =
      struct
        let info =
          let doc = "Build sources" in
          let man = [
            `P "Builds a native executable from a Nile source file.";

            `S Manpage.s_options;
            `S Args.OptArgs.docs;
            `S Args.ClosArgs.docs;
            `S Args.DumpArgs.docs;
          ]
          in
          Term.info "build" ~doc ~exits:Term.default_exits ~man

        let src =
          let doc = "Input source file" in
          let docv = "SOURCE-FILE" in
          Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)

        let exe =
          let doc = "Output executable file" in
          let docv = "TARGET-EXE" in
          Arg.(value & opt (some string) None & info ["o"; "output"] ~doc ~docv)

        let compile src exe config =
          let _ = exe in

          let ast =
            src
              |> Syntax.Lexer.from_file
              |> Syntax.Parser.top Syntax.Lexer.lex
          in

          let _ =
            if config.Args.CompilerArgs.dump.annot_ast
            then
              let iter ast = Syntax.Top.Ast.pp ast err_formatter in
              List.iter iter ast
            else ()
          in

          ()

        let term = Term.(const compile $ src $ exe $ Args.CompilerArgs.term)

        let cmd = (term, info)
      end
  end

(* Top level *)

let main argv =
  Term.exit @@ Term.eval_choice ~argv Cmds.DefaultCmd.cmd [Cmds.BuildCmd.cmd]
