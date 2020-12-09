open OUnit2
open Nile

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
  let assert_loc ~ctxt fname start_pos end_pos len actual =
    let expected = Loc.mock fname start_pos end_pos in
    assert_equal ~ctxt ~cmp:Loc.equal expected actual;
    assert_equal ~ctxt len actual.length
  in

  let test_dummy =
    let test_dummy ctxt =
      Loc.dummy
        |> assert_loc ~ctxt "" (-1, -1, -1) (-1, -1, -1) (-1)
    in
    "Dummy Location" >::: [
      "Dummy Location" >:: test_dummy;
    ]
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
      let start_loc =
        mock_lexbuf fname (1, 10, 15) (1, 10, 20)
          |> Loc.loc
      in
      let end_loc =
        mock_lexbuf fname (1, 10, 15) (3, 100, 105)
          |> Loc.loc
      in

      Loc.span start_loc end_loc
        |> assert_loc ~ctxt fname (1, 5, 15) (3, 5, 105) 90
    in
    let test_dummy =
      let real = Loc.mock "" (1, 2, 3) (4, 5, 6) in

      let test_left ctxt =
        let actual = Loc.span Loc.dummy real in
        assert_equal ~ctxt (-1) actual.length
      in
      let test_right ctxt =
        let actual = Loc.span real Loc.dummy in
        assert_equal ~ctxt (-1) actual.length
      in
      let test_both ctxt =
        let actual = Loc.span Loc.dummy Loc.dummy in
        assert_equal ~ctxt (-1) actual.length
      in
      "Dummy Locations" >::: [
        "Left"  >:: test_left;
        "Right" >:: test_right;
        "Both"  >:: test_both
      ]
    in
    let test_mismatched_filenames _ =
      let fname_1 = "file-1.nile" in
      let start_loc =
        mock_lexbuf fname_1 (1, 10, 15) (1, 10, 20)
          |> Loc.loc
      in

      let fname_2 = "file-2.nile" in
      let end_loc =
        mock_lexbuf fname_2 (1, 10, 15) (1, 10, 20)
          |> Loc.loc
      in

      let exn = Loc.MismatchedFileNames (fname_1, fname_2) in
      let fn _ =
        let _ = Loc.span start_loc end_loc in
        ()
      in
      assert_raises exn fn
    in
    "Merging Locations" >::: [
      "Spanning Locations"        >:: test_span;
      test_dummy;
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
  let test_equal =
    let loc_not_equal x y = not (Loc.equal x y) in
    let loc = Loc.mock "fname" (1, 2, 3) (4, 5, 6) in

    let test_equal ctxt =
      Loc.mock "fname" (1, 2, 3) (4, 5, 6)
        |> assert_equal ~ctxt ~cmp:Loc.equal loc
    in
    let test_fname ctxt =
      Loc.mock "different-fname" (1, 2, 3) (4, 5, 6)
        |> assert_equal ~ctxt ~cmp:loc_not_equal loc
    in
    let test_start_pos =
      let test_line ctxt =
        Loc.mock "fname" (42, 2, 3) (4, 5, 6)
          |> assert_equal ~ctxt ~cmp:loc_not_equal loc
      in
      let test_col ctxt =
        Loc.mock "fname" (1, 42, 3) (4, 5, 6)
          |> assert_equal ~ctxt ~cmp:loc_not_equal loc
      in
      let test_off ctxt =
        Loc.mock "fname" (1, 2, 42) (4, 5, 6)
          |> assert_equal ~ctxt ~cmp:loc_not_equal loc
      in
      "Start Position" >::: [
        "Different Line"   >:: test_line;
        "Different Column" >:: test_col;
        "Different Offset" >:: test_off;
      ]
    in
    let test_end_pos =
    let test_line ctxt =
      Loc.mock "fname" (1, 2, 3) (42, 5, 6)
        |> assert_equal ~ctxt ~cmp:loc_not_equal loc
    in
    let test_col ctxt =
      Loc.mock "fname" (1, 2, 3) (4, 42, 6)
        |> assert_equal ~ctxt ~cmp:loc_not_equal loc
    in
    let test_off ctxt =
      Loc.mock "fname" (1, 2, 3) (4, 5, 42)
        |> assert_equal ~ctxt ~cmp:loc_not_equal loc
    in
      "End Position" >::: [
        "Different Line"   >:: test_line;
        "Different Column" >:: test_col;
        "Different Offset" >:: test_off;
      ]
    in
    "Equality" >::: [
      "Success"            >:: test_equal;
      "Different Filename" >:: test_fname;
      test_start_pos;
      test_end_pos;
    ]
  in
  "Location Tracking" >::: [
    test_dummy;
    test_mock;
    test_loc;
    test_span;
    test_track;
    test_equal;
  ]
