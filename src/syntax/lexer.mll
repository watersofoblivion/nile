{
  open Parser

  let from_string src str =
    let lexbuf = Lexing.from_string str in
    Loc.track "-" lexbuf;
    lexbuf

  let from_channel src ic =
    let lexbuf = Lexing.from_channel ic in
    Loc.track fname lexbuf;
    lexbuf

  let from_file src path =
    path
      |> open_in
      |> from_channel src

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

  let op_band lexbuf =
    let loc = Loc.loc lexbuf in
    BAND loc
  let op_bxor lexbuf =
    let loc = Loc.loc lexbuf in
    BXOR loc
  let op_bnot lexbuf =
    let loc = Loc.loc lexbuf in
    BNOT loc
  let op_lsl lexbuf =
    let loc = Loc.loc lexbuf in
    LSL loc
  let op_lsr lexbuf =
    let loc = Loc.loc lexbuf in
    LSR loc
  let op_asl lexbuf =
    let loc = Loc.loc lexbuf in
    ASL loc
  let op_asr lexbuf =
    let loc = Loc.loc lexbuf in
    ASR loc
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
  let op_land lexbuf =
    let loc = Loc.loc lexbuf in
    LAND loc
  let op_lor lexbuf =
    let loc = Loc.loc lexbuf in
    LOR loc
  let op_lnot lexbuf =
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

  let lit_unit lexbuf =
    let loc = Loc.loc lexbuf in
    UNIT loc
  let lit_bool lexbuf =
    let loc = Loc.loc lexbuf in
    BOOL (loc, Lexing.lexeme lexbuf)
  let lit_int radix lexbuf =
    let loc = Loc.loc lexbuf in
    INT (loc, radix, Lexing.lexeme lexbuf)
  let lit_float hex lexbuf =
    let loc = Loc.loc lexbuf in
    FLOAT (loc, hex, Lexing.lexeme lexbuf)
  let lit_rune lexbuf =
    let loc = Loc.loc lexbuf in
    RUNE (loc, Lexing.lexeme lexbuf)
  let lit_string lexbuf =
    let loc = Loc.loc lexbuf in
    STRING (loc, Lexing.lexeme lexbuf)
  let lit_byte radix lexbuf =
    let loc = Loc.loc lexbuf in
    BYTE (loc, radix, Lexing.lexeme lexbuf)
  let lit_blob lexbuf =
    let loc = Loc.loc lexbuf in
    BLOB (loc, Lexing.lexeme lexbuf)
  let lit_timestamp lexbuf =
    let loc = Loc.loc lexbuf in
    TIMESTAMP (loc, Lexing.lexeme lexbuf)
  let lit_duration lexbuf =
    let loc = Loc.loc lexbuf in
    DURATION (loc, Lexing.lexeme lexbuf)

  let uident lexbuf =
    let loc = Loc.loc lexbuf in
    UIDENT (loc, Lexing.lexeme lexbuf)
  let lident lexbuf =
    let loc = Loc.loc lexbuf in
    LIDENT (loc, Lexing.lexeme lexbuf)
}

rule lex src = parse
(* Non-printable *)
| eof        { eof lexbuf }

(* Whitespace *)
| [' ' '\t'] { lex src lexbuf }
| '\n'       { newline (lex src) lexbuf }

(* Punctuation *)
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

(* Keywords *)
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

(* Operators *)
| '&'   { op_band lexbuf }
| '^'   { op_bxor lexbuf }
| '~'   { op_bnot lexbuf }
| "<<"  { op_lsl lexbuf }
| ">>"  { op_lsr lexbuf }
| "<<<" { op_asl lexbuf }
| ">>>" { op_asr lexbuf }
| '+'   { op_add lexbuf }
| '-'   { op_sub lexbuf }
| '*'   { op_mul lexbuf }
| '/'   { op_div lexbuf }
| '%'   { op_mod lexbuf }
| "&&"  { op_and lexbuf }
| "||"  { op_or lexbuf }
| "!"   { op_not lexbuf }
| "=="  { op_eq lexbuf }
| "!="  { op_neq lexbuf }
| "<="  { op_lte lexbuf }
| "<"   { op_lt lexbuf }
| ">"   { op_gt lexbuf }
| ">="  { op_gte lexbuf }
| '.'   { op_dot lexbuf }
| "::"  { op_cons lexbuf }

(* Literals *)

(* Unit *)
| "()" { lit_unit lexbuf }

(* Boolean *)
| "true"  { lit_bool lexbuf }
| "false" { lit_bool lexbuf }

(* Integer *)
| ('+'|'-')?  '0' ['B' 'b']   ['0'-'1']+                 { lit_int 2 lexbuf }
| ('+'|'-')?  '0' ['O' 'o']   ['0'-'7']+                 { lit_int 8 lexbuf }
| ('+'|'-')? ('0' ['D' 'd'])? ['0'-'9']+                 { lit_int 10 lexbuf }
| ('+'|'-')?  '0' ['X' 'x']   ['0'-'9' 'A'-'F' 'a'-'f']+ { lit_int 16 lexbuf }

(* Floating-Point *)
| ('+'|'-')?['0'-'9']+'.'['0'-'9']+(['E' 'e']('+'|'-')?['0'-'9']+)? { lit_float false lexbuf }
| (['F' 'f'] ['0'-'9' 'A'-'F' 'a'-'f']+                             { lit_float true lexbuf }

(* Rune *)
| "'" ("\\'" | "\\u" ['0'-'9' 'A'-'F' 'a'-'f']+ | [^'\'' '\n']) "'" { lit_rune lexbuf }

(* String *)
| '"' ("\\\"" | "\\u" ['0'-'9' 'A'-'F' 'a'-'f']+ | [^'"' '\n'])* '"' { lit_string lexbuf }

(* Byte *)
|  "\\" ['B' 'b']   ['0'-'1']+                 { lit_byte 2 lexbuf }
|  "\\" ['O' 'o']   ['0'-'7']+                 { lit_byte 8 lexbuf }
| ("\\" ['D' 'd'])? ['0'-'9']+                 { lit_byte 10 lexbuf }
|  "\\" ['X' 'x']?  ['0'-'9' 'A'-'F' 'a'-'f']+ { lit_byte 10 lexbuf }

(* Blob *)
| '`' ( "\\" ['B' 'b']   ['0'-'1']+)? '`'                 { lit_blob 2 lexbuf }
| '`' ( "\\" ['O' 'o']   ['0'-'7']+)? '`'                 { lit_blob 8 lexbuf }
| '`' (("\\" ['D' 'd'])? ['0'-'9']+)? '`'                 { lit_blob 10 lexbuf }
| '`' ( "\\" ['X' 'x']   ['0'-'9' 'A'-'F' 'a'-'f']+)? '`' { lit_blob 16 lexbuf }

(* Timestamp *)
| "@" (['0'-'9']+ '-' ['0'-'9']+ '-' ['0'-'9']+)? ("T" ['0'-'9']+ ':' ['0'-'9']+ ':' ['0'-'9']+ ('Z' | (['+' '-'] ['0'-'9']+ ':' ['0'-'9']))?)? { lit_timestamp lexbuf }

(* Duration *)
| "@@" (['0'-'9']+"Y")? (['0'-'9']+"M")? (['0'-'9']+"D")? ("T" (['0'-'9']+"H")? (['0'-'9']+"M")? (['0'-'9']+"S")?)? { lit_duration lexbuf }

(* Identifiers *)
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* { lident lexbuf }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* { uident lexbuf }
