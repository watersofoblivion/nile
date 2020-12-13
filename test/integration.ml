open OUnit2
(* open NileIntegration *)

let _ =
  "Nile" >::: [
    "Integration Tests" >::: [
    ]
  ]
    |> run_test_tt_main
