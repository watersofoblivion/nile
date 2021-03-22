open OUnit2

let _ =
  "Nile" >::: [
    "Unit Tests" >::: [
      "Common" >::: [
        CommonTest.LocTest.suite;
        CommonTest.SymTest.suite;
      ];
      "Abstract Syntax" >::: [
        SyntaxTest.OpTest.suite;
        SyntaxTest.LexerTest.suite;
        SyntaxTest.ParserTest.suite;
        SyntaxTest.TypeTest.suite;
        SyntaxTest.PattTest.suite;
        SyntaxTest.AstTest.suite;
        SyntaxTest.PrettyTest.suite;
      ];
      (* "Annotated Syntax" >::: [
        AnnotTest.OpTest.suite;
        AnnotTest.TypeTest.suite;
        AnnotTest.PattTest.suite;
        AnnotTest.AstTest.suite;
        AnnotTest.AnnotateTest.suite;
        AnnotTest.CheckTest.suite;
        AnnotTest.PrettyTest.suite;
      ];
      "Intermediate Representation" >::: [
        IrTest.TypeTest.suite;
        IrTest.PattTest.suite;
        IrTest.BuiltinTest.suite;
        IrTest.AnfTest.suite;
        IrTest.NormalizeTest.suite;
        IrTest.CheckTest.suite;
        IrTest.PrettyTest.suite;
        IrTest.SerdeTest.suite
      ];
      "Code Generation" >::: [
        CodegenTest.ClosTest.suite;
        CodegenTest.AsmTest.suite;
      ];
      "Command-Line Interface" >::: [
        CliTest.suite;
      ] *)
    ]
  ]
    |> run_test_tt_main
