open OUnit2
open NileUnit

let _ =
  "Nile" >::: [
    "Unit Tests" >::: [
      TopTest.suite;
      LocTest.suite;
      LexerTest.suite;
      ParserTest.suite;
      TypeTest.suite;
      OpTest.suite;
      AstTest.suite;
      CheckTest.suite;
      AnfTest.suite;
      ClosTest.suite;
      CodegenTest.suite;
      CliTest.suite;
    ]
  ]
    |> run_test_tt_main
