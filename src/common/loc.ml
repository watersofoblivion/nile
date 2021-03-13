open Common

type pos = {
  line: int;
  col:  int;
  off:  int
}

let pos pos = {
  line = pos.Lexing.pos_lnum;
  col  = pos.pos_cnum - pos.pos_bol;
  off  = pos.pos_cnum
}

type src = {
  mojule:  string;
  major:   int;
  package: string;
  file:    string
}

let src mojule major package file = { mojule; major; package; file }

type t = {
  src:       src;
  start_pos: pos;
  end_pos:   pos;
  length:    int
}

let mock mojule package file (start_line, start_col, start_off) (end_line, end_col, end_off) =
  let length =
    if start_off < 0 || end_off < 0
    then -1
    else end_off - start_off
  in
  { fname     = fname;
    start_pos = { line = start_line;
                  col  = start_col;
                  off  = start_off };
    end_pos   = { line = end_line;
                  col  = end_col;
                  off  = end_off };
    length    = length }

let loc lexbuf =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  { fname     = start_p.pos_fname;
    start_pos = pos start_p;
    end_pos   = pos end_p;
    length    = end_p.pos_cnum - start_p.pos_cnum }

exception MismatchedFileNames of string * string

let span start_loc end_loc =
  if start_loc.fname <> end_loc.fname
  then
    MismatchedFileNames (start_loc.fname, end_loc.fname)
      |> raise
  else
    let length =
      if start_loc.start_pos.off < 0 || end_loc.end_pos.off < 0
      then -1
      else end_loc.end_pos.off - start_loc.start_pos.off
    in
    { fname     = start_loc.fname;
      start_pos = start_loc.start_pos;
      end_pos   = end_loc.end_pos;
      length    = length }

let track fname lexbuf =
  let pos =
    { Lexing.pos_fname = "";
      pos_lnum         = 1;
      pos_bol          = 0;
      pos_cnum         = 0 }
  in
  lexbuf.Lexing.lex_start_p <- { pos with pos_fname = fname };
  lexbuf.lex_curr_p         <- { pos with pos_fname = fname }
