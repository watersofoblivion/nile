{
  open Parser

  let from_string str =
    let lexbuf = Lexing.from_string str in
    Loc.track "-" lexbuf;
    lexbuf

  let from_channel ?fname:(fname = "") ic =
    let lexbuf = Lexing.from_channel ic in
    Loc.track fname lexbuf;
    lexbuf

  let from_file path =
    path
      |> open_in
      |> from_channel ~fname:path

  let eof lexbuf =
    let loc = Loc.loc lexbuf in
    EOF loc
  let newline lex lexbuf =
    Lexing.new_line lexbuf;
    lex lexbuf

  let punc_lparen lexbuf =
    let loc = Loc.loc lexbuf in
    LPAREN loc
  let punc_rparen lexbuf =
    let loc = Loc.loc lexbuf in
    RPAREN loc
  let punc_colon lexbuf =
    let loc = Loc.loc lexbuf in
    COLON loc
  let punc_arrow lexbuf =
    let loc = Loc.loc lexbuf in
    ARROW loc
  let punc_darrow lexbuf =
    let loc = Loc.loc lexbuf in
    DARROW loc
  let punc_bind lexbuf =
    let loc = Loc.loc lexbuf in
    BIND loc
  let punc_comma lexbuf =
    let loc = Loc.loc lexbuf in
    COMMA loc

  let kwd_let lexbuf =
    let loc = Loc.loc lexbuf in
    LET loc
  let kwd_rec lexbuf =
    let loc = Loc.loc lexbuf in
    REC loc
  let kwd_and lexbuf =
    let loc = Loc.loc lexbuf in
    AND loc
  let kwd_in lexbuf =
    let loc = Loc.loc lexbuf in
    IN loc
  let kwd_if lexbuf =
    let loc = Loc.loc lexbuf in
    IF loc
  let kwd_then lexbuf =
    let loc = Loc.loc lexbuf in
    THEN loc
  let kwd_else lexbuf =
    let loc = Loc.loc lexbuf in
    ELSE loc

  let op_add lexbuf =
    let loc = Loc.loc lexbuf in
    ADD loc
  let op_sub lexbuf =
    let loc = Loc.loc lexbuf in
    SUB loc
  let op_mul lexbuf =
    let loc = Loc.loc lexbuf in
    MUL loc
  let op_div lexbuf =
    let loc = Loc.loc lexbuf in
    DIV loc
  let op_mod lexbuf =
    let loc = Loc.loc lexbuf in
    MOD loc
  let op_and lexbuf =
    let loc = Loc.loc lexbuf in
    LAND loc
  let op_or lexbuf =
    let loc = Loc.loc lexbuf in
    LOR loc
  let op_not lexbuf =
    let loc = Loc.loc lexbuf in
    LNOT loc
  let op_eq lexbuf =
    let loc = Loc.loc lexbuf in
    EQ loc
  let op_neq lexbuf =
    let loc = Loc.loc lexbuf in
    NEQ loc
  let op_lte lexbuf =
    let loc = Loc.loc lexbuf in
    LTE loc
  let op_lt lexbuf =
    let loc = Loc.loc lexbuf in
    LT loc
  let op_gt lexbuf =
    let loc = Loc.loc lexbuf in
    GT loc
  let op_gte lexbuf =
    let loc = Loc.loc lexbuf in
    GTE loc

  let lit_true lexbuf =
    let loc = Loc.loc lexbuf in
    BOOL (loc, true)
  let lit_false lexbuf =
    let loc = Loc.loc lexbuf in
    BOOL (loc, false)
  let lit_int lexbuf =
    let i =
      lexbuf
        |> Lexing.lexeme
        |> int_of_string
    in
    let loc = Loc.loc lexbuf in
    INT (loc, i)

  let uident lexbuf =
    let loc = Loc.loc lexbuf in
    UIDENT (loc, Lexing.lexeme lexbuf)
  let lident lexbuf =
    let loc = Loc.loc lexbuf in
    LIDENT (loc, Lexing.lexeme lexbuf)
}

rule lex = parse
| [' ' '\t'] { lex lexbuf }
| '\n'       { newline lex lexbuf }

| '('   { punc_lparen lexbuf }
| ')'   { punc_rparen lexbuf }
| "->"  { punc_arrow lexbuf }
| "=>"  { punc_darrow lexbuf }
| ':'   { punc_colon lexbuf }
| '='   { punc_bind lexbuf }
| ','   { punc_comma lexbuf }

| "let"  { kwd_let lexbuf }
| "rec"  { kwd_rec lexbuf }
| "and"  { kwd_and lexbuf }
| "in"   { kwd_in lexbuf }
| "if"   { kwd_if lexbuf }
| "then" { kwd_then lexbuf }
| "else" { kwd_else lexbuf }

| '+'  { op_add lexbuf }
| '-'  { op_sub lexbuf }
| '*'  { op_mul lexbuf }
| '/'  { op_div lexbuf }
| '%'  { op_mod lexbuf }
| "&&" { op_and lexbuf }
| "||" { op_or lexbuf }
| "!"  { op_not lexbuf }
| "==" { op_eq lexbuf }
| "!=" { op_neq lexbuf }
| "<=" { op_lte lexbuf }
| "<"  { op_lt lexbuf }
| ">"  { op_gt lexbuf }
| ">=" { op_gte lexbuf }

| "true"               { lit_true lexbuf }
| "false"              { lit_false lexbuf }
| ('+'|'-')?['0'-'9']+ { lit_int lexbuf }

| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* { lident lexbuf }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* { uident lexbuf }

| eof { eof lexbuf }
