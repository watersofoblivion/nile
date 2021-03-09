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
  let punc_lbracket lexbuf =
    let loc = Loc.loc lexbuf in
    LBRACKET loc
  let punc_rbracket lexbuf =
    let loc = Loc.loc lexbuf in
    RBRACKET loc
  let punc_lbrace lexbuf =
    let loc = Loc.loc lexbuf in
    LBRACE loc
  let punc_rbrace lexbuf =
    let loc = Loc.loc lexbuf in
    RBRACE loc
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
  let punc_ground lexbuf =
    let loc = Loc.loc lexbuf in
    GROUND loc
  let punc_pipe lexbuf =
    let loc = Loc.loc lexbuf in
    PIPE loc
  let punc_elipsis lexbuf =
    let loc = Loc.loc lexbuf in
    ELIPSIS loc

  let kwd_type lexbuf =
    let loc = Loc.loc lexbuf in
    TYPE loc
  let kwd_val lexbuf =
    let loc = Loc.loc lexbuf in
    VAL loc
  let kwd_def lexbuf =
    let loc = Loc.loc lexbuf in
    DEF loc
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
  let kwd_case lexbuf =
    let loc = Loc.loc lexbuf in
    CASE loc
  let kwd_of lexbuf =
    let loc = Loc.loc lexbuf in
    OF loc
  let kwd_end lexbuf =
    let loc = Loc.loc lexbuf in
    END loc
  let kwd_from lexbuf =
    let loc = Loc.loc lexbuf in
    FROM loc
  let kwd_import lexbuf =
    let loc = Loc.loc lexbuf in
    IMPORT loc
  let kwd_package lexbuf =
    let loc = Loc.loc lexbuf in
    PACKAGE loc
  let kwd_as lexbuf =
    let loc = Loc.loc lexbuf in
    AS loc

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
  let op_dot lexbuf =
    let loc = Loc.loc lexbuf in
    DOT loc
  let op_const lexbuf =
    let loc = Loc.loc lexbuf in
    CONS loc

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
  let lit_float lexbuf =
    let f =
      lexbuf
        |> Lexing.lexeme
        |> float_of_string
    in
    let loc = Loc.loc lexbuf in
    FLOAT (loc, f)
  let lit_string lexbuf =
    let s =
      let lexeme = Lexing.lexeme lexbuf in
      String.sub lexeme 1 (String.length lexeme - 2)
    in
    let loc = Loc.loc lexbuf in
    STRING (loc, s)
  let lit_blob lexbuf =
    let b =
      let lexeme = Lexing.lexeme lexbuf in
      String.sub lexeme 1 (String.length lexeme - 2)
    in
    let loc = Loc.loc lexbuf in
    BLOB (loc, b)
  let lit_timestamp lexbuf =
    let ts =
      let lexeme = Lexing.lexeme lexbuf in
      String.sub lexeme 1 (String.length lexeme - 1)
    in
    let loc = Loc.loc lexbuf in
    TIMESTAMP (loc, ts)
  let lit_duration lexbuf =
    let d =
      let lexeme = Lexing.lexeme lexbuf in
      String.sub lexeme 2 (String.length lexeme - 2)
    in
    let loc = Loc.loc lexbuf in
    DURATION (loc, d)

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
| '['   { punc_lbracket lexbuf }
| ']'   { punc_rbracket lexbuf }
| '{'   { punc_lbrace lexbuf }
| '}'   { punc_rbrace lexbuf }
| "->"  { punc_arrow lexbuf }
| "=>"  { punc_darrow lexbuf }
| ':'   { punc_colon lexbuf }
| '='   { punc_bind lexbuf }
| ','   { punc_comma lexbuf }
| '_'   { punc_ground lexbuf }
| '|'   { punc_pipe lexbuf }
| "..." { punc_elipsis lexbuf }

| "type"    { kwd_type lexbuf }
| "val"     { kwd_type lexbuf }
| "def"     { kwd_type lexbuf }
| "let"     { kwd_let lexbuf }
| "rec"     { kwd_rec lexbuf }
| "and"     { kwd_and lexbuf }
| "in"      { kwd_in lexbuf }
| "if"      { kwd_if lexbuf }
| "then"    { kwd_then lexbuf }
| "else"    { kwd_else lexbuf }
| "case"    { kwd_case lexbuf }
| "of"      { kwd_of lexbuf }
| "end"     { kwd_end lexbuf }
| "from"    { kwd_from lexbuf }
| "import"  { kwd_import lexbuf }
| "package" { kwd_package lexbuf }
| "as"      { kwd_package lexbuf }

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
| '.'  { op_dot lexbuf }
| "::" { op_cons lexbuf }

| "true"               { lit_true lexbuf }
| "false"              { lit_false lexbuf }
| ('+'|'-')?['0'-'9']+'.'['0'-'9']+(['E' 'e']('+'|'-')?['0'-'9']+)? { lit_float lexbuf }
| ('+'|'-')?['0'-'9']+ { lit_int lexbuf }
| '"' ("\\\"" | [^'"' '\n'])* '"' { lit_string lexbuf }
| '`' ("\\" 'u'? ['0'-'9']+)* '`' { lit_blob lexbuf }
| "@" (['0'-'9']+ '-' ['0'-'9']+ '-' ['0'-'9']+)? ("T" ['0'-'9']+ ':' ['0'-'9']+ ':' ['0'-'9']+ ('Z' | (['+' '-'] ['0'-'9']+ ':' ['0'-'9']))?)? { lit_timestamp lexbuf }
| "@@P" (['0'-'9']+"Y")? (['0'-'9']+"M")? (['0'-'9']+"D")? ("T" (['0'-'9']+"H")? (['0'-'9']+"M")? (['0'-'9']+"S")?)? { lit_duration lexbuf }

| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* { lident lexbuf }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* { uident lexbuf }

| eof { eof lexbuf }
