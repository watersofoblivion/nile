open OUnit2
open Syntax

let dummy = Loc.mock "-" (-1, -1, -1) (-1, -1, -1)

let gen =
  let idx = ref 0 in
  let next _ =
    incr idx;
    !idx
  in
  (fun _ -> Loc.mock "-" (next (), next (), next ()) (next (), next (), next ()))

let assert_loc ~ctxt fname (start_line, start_col, start_off) (end_line, end_col, end_off) len actual =
  assert_equal ~ctxt ~printer:Fun.id fname actual.Loc.fname ~msg:"Location filenames are different";
  assert_equal ~ctxt ~printer:string_of_int start_line actual.start_pos.line ~msg:"Location start lines are different";
  assert_equal ~ctxt ~printer:string_of_int start_col actual.start_pos.col ~msg:"Location start columns are different";
  assert_equal ~ctxt ~printer:string_of_int start_off actual.start_pos.off ~msg:"Location start offsets are different";
  assert_equal ~ctxt ~printer:string_of_int end_line actual.end_pos.line ~msg:"Location end lines are different";
  assert_equal ~ctxt ~printer:string_of_int end_col actual.end_pos.col ~msg:"Location end columns are different";
  assert_equal ~ctxt ~printer:string_of_int end_off actual.end_pos.off ~msg:"Location end offsets are different";
  assert_equal ~ctxt ~printer:string_of_int len actual.length ~msg:"Location lengths are different"

let assert_loc_equal ~ctxt expected actual =
  assert_loc ~ctxt
    expected.Loc.fname
    (expected.start_pos.line, expected.start_pos.col, expected.start_pos.off)
    (expected.end_pos.line, expected.end_pos.col, expected.end_pos.off)
    expected.length
    actual

let suite =
  let mock_lexbuf fname (start_lnum, start_bol, start_cnum) (end_lnum, end_bol, end_cnum) =
    let pos lnum bol cnum =
      { Lexing.pos_fname = fname;
        pos_lnum         = lnum;
        pos_bol          = bol;
        pos_cnum         = cnum }
    in
    let lexbuf = Lexing.from_string "" in
    lexbuf.lex_start_p <- pos start_lnum start_bol start_cnum ;
    lexbuf.lex_curr_p <- pos end_lnum end_bol end_cnum ;
    lexbuf
  in

  let test_mock =
    let fname = "filename.nile" in
    let start_line = 1 in
    let start_col = 2 in
    let start_off = 3 in
    let end_line = 4 in
    let end_col = 5 in
    let end_off = 6 in

    let test_mock ctxt =
      Loc.mock fname (start_line, start_col, start_off) (end_line, end_col, end_off)
        |> assert_loc ~ctxt fname (start_line, start_col, start_off) (end_line, end_col, end_off) 3
    in
    let test_neg_start ctxt =
      Loc.mock fname (start_line, start_col, -1) (end_line, end_col, end_off)
        |> assert_loc ~ctxt fname (start_line, start_col, -1) (end_line, end_col, end_off) (-1)
    in
    let test_neg_end ctxt =
      Loc.mock fname (start_line, start_col, start_off) (end_line, end_col, -1)
        |> assert_loc ~ctxt fname (start_line, start_col, start_off) (end_line, end_col, -1) (-1)
    in
    "Mock Location" >::: [
      "Mock Location"              >:: test_mock;
      "With Negative Start Offset" >:: test_neg_start;
      "With Negative End Offset"   >:: test_neg_end;
    ]
  in
  let test_loc =
    let test_loc ctxt =
      let fname = "test-filename.nile" in
      mock_lexbuf fname (1, 10, 15) (1, 10, 20)
        |> Loc.loc
        |> assert_loc ~ctxt fname (1, 5, 15) (1, 10, 20) 5
    in
    "From Lexer" >::: [
      "Of Current Lexeme" >:: test_loc;
    ]
  in
  let test_span =
    let test_span ctxt =
      let fname = "test-filename.nile" in
      let start_loc = Loc.mock fname (1, 10, 15) (1, 10, 20) in
      let end_loc = Loc.mock fname (1, 10, 15) (3, 100, 105) in

      Loc.span start_loc end_loc
        |> assert_loc ~ctxt fname (1, 10, 15) (3, 100, 105) 90
    in
    let test_negative_offsets =
      let fname = "test-filename.nile" in

      let test_start ctxt =
        let start_loc = Loc.mock fname (-1, -1, -1) (-1, -1, 0) in
        let end_loc = Loc.mock fname (-1, -1, -1) (-1, -1, 0) in

        Loc.span start_loc end_loc
          |> assert_loc ~ctxt fname (-1, -1, -1) (-1, -1, 0) (-1)
      in
      let test_end ctxt =
        let start_loc = Loc.mock fname (-1, -1, 0) (-1, -1, -1) in
        let end_loc = Loc.mock fname (-1, -1, 0) (-1, -1, -1) in

        Loc.span start_loc end_loc
          |> assert_loc ~ctxt fname (-1, -1, 0) (-1, -1, -1) (-1)
      in
      "Negative Offsets" >::: [
        "Starting Location" >:: test_start;
        "Ending Location"   >:: test_end;
      ]
    in
    let test_mismatched_filenames _ =
      let fname_1 = "file-1.nile" in
      let start_loc = Loc.mock fname_1 (1, 10, 15) (1, 10, 20) in

      let fname_2 = "file-2.nile" in
      let end_loc = Loc.mock fname_2 (1, 10, 15) (1, 10, 20) in

      let exn = Loc.MismatchedFileNames (fname_1, fname_2) in
      let fn _ =
        let _ = Loc.span start_loc end_loc in
        ()
      in
      assert_raises exn fn
    in
    "Merging Locations" >::: [
      "Spanning Locations"        >:: test_span;
      test_negative_offsets;
      "With Mismatched Filenames" >:: test_mismatched_filenames
    ]
  in
  let test_track =
    let test_track ctxt =
      let fname = "filename.nile" in
      let lexbuf = Lexing.from_string "" in
      Loc.track fname lexbuf;

      assert_equal ~ctxt fname lexbuf.lex_start_p.pos_fname;
      assert_equal ~ctxt 1 lexbuf.lex_start_p.pos_lnum;
      assert_equal ~ctxt 0 lexbuf.lex_start_p.pos_bol;
      assert_equal ~ctxt 0 lexbuf.lex_start_p.pos_cnum;

      assert_equal ~ctxt fname lexbuf.lex_curr_p.pos_fname;
      assert_equal ~ctxt 1 lexbuf.lex_curr_p.pos_lnum;
      assert_equal ~ctxt 0 lexbuf.lex_curr_p.pos_bol;
      assert_equal ~ctxt 0 lexbuf.lex_curr_p.pos_cnum
    in
    "Enable" >::: [
      "Enable" >:: test_track;
    ]
  in
  "Location Tracking" >::: [
    test_mock;
    test_loc;
    test_span;
    test_track;
  ]
