open Cmdliner

let main _ =
  let info =
    let doc = "Compiler for the Nile language" in
    let man = [
      `P "Compiler for the Nile language"
    ]
    in
    Term.info "nilec" ~doc ~exits:Term.default_exits ~man
  in

  let term =
    let dump_all =
      let doc = "Print all of the intermediate forms to stderr" in
      Arg.(value & flag & info ["dump-all"] ~doc)
    in

    let compile dump_all =
      Printf.eprintf "Compiling...\n";
      if dump_all
      then Printf.eprintf "Dumping all forms...\n"
      else ()
    in

    Term.(const compile $ dump_all)
  in

  Term.exit @@ Term.eval (term, info)
