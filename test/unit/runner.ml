open OUnit2

let _ =
  "Nile" >::: [
    "Unit Tests" >::: [
      "Common" >::: [
        CommonTest.SymTest.suite;
        CommonTest.PattTest.suite;
        CommonTest.TypeTest.suite;
        CommonTest.OpTest.suite;
        CommonTest.BuiltinTest.suite;
      ];
      "Syntax" >::: [
        SyntaxTest.LocTest.suite;
        SyntaxTest.LexerTest.suite;
        SyntaxTest.ParserTest.suite;
        SyntaxTest.AstTest.suite;
        SyntaxTest.AnnotTest.suite;
      ];
      "Intermediate Representation" >::: [
        IrTest.AnfTest.suite;
        IrTest.OptTest.suite;
      ];
      "Code Generation" >::: [
        CodegenTest.ClosTest.suite;
        CodegenTest.AsmTest.suite;
      ];
      "Command-Line Interface" >::: [
        CliTest.suite;
      ]
    ]
  ]
    |> run_test_tt_main
