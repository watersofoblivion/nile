open OUnit2

let _ =
  "Nile" >::: [
    "Unit Tests" >::: [
      "Common" >::: [
        CommonTest.TypeTest.suite;
        CommonTest.OpTest.suite;
      ];
      "Syntax" >::: [
        SyntaxTest.LocTest.suite;
        SyntaxTest.LexerTest.suite;
        SyntaxTest.ParserTest.suite;
        SyntaxTest.AstTest.suite;
        SyntaxTest.PrettyTest.suite;
        SyntaxTest.CheckTest.suite;
      ];
      "Intermediate Representation" >::: [
        IrTest.AnfTest.suite;
        IrTest.NormalizeTest.suite;
        IrTest.PrettyTest.suite;
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
