open Format
open OUnit2

let never _ _ = false

let printer pp x =
  x
    |> pp
    |> fprintf str_formatter "%t"
    |> flush_str_formatter

let assert_pp pp ~ctxt lines expr =
  let printer str =
    let map str = sprintf "%S" str in
    str
      |> String.split_on_char '\n'
      |> List.map map
      |> String.concat "\n"
  in
  let expected = String.concat "\n" lines in
  fprintf str_formatter "%t" (pp expr)
    |> flush_str_formatter
    |> assert_equal ~ctxt ~printer expected
