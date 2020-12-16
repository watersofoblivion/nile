open OUnit2

let _ =
  "Nile" >::: [
    "Unit Tests" >::: [
      "Syntax" >::: [
        SyntaxTest.TopTest.suite;
        SyntaxTest.LocTest.suite;
        SyntaxTest.LexerTest.suite;
        SyntaxTest.ParserTest.suite;
        SyntaxTest.TypeTest.suite;
        SyntaxTest.OpTest.suite;
        SyntaxTest.AstTest.suite;
        SyntaxTest.CheckTest.suite;
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
