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

  let equal t t' = match t, t' with
    | EOF loc, EOF loc'
    | LPAREN loc, LPAREN loc' | RPAREN loc, RPAREN loc'
    | COLON loc, COLON loc' | ARROW loc, ARROW loc' | BIND loc, BIND loc' | COMMA loc, COMMA loc'
    | LET loc, LET loc' | REC loc, REC loc' | AND loc, AND loc' | IN loc, IN loc'
    | IF loc, IF loc' | THEN loc, THEN loc' | ELSE loc, ELSE loc'
    | ADD loc, ADD loc' | SUB loc, SUB loc' | MUL loc, MUL loc' | DIV loc, DIV loc' | MOD loc, MOD loc'
    | LAND loc, LAND loc' | LOR loc, LOR loc' | LNOT loc, LNOT loc'
    | EQ loc, EQ loc' | NEQ loc, NEQ loc'
    | LTE loc, LTE loc' | LT loc, LT loc' | GT loc, GT loc' | GTE loc, GTE loc' -> Loc.equal loc loc'
    | BOOL (loc, b), BOOL (loc', b') -> Loc.equal loc loc' && b = b'
    | INT (loc, i), INT (loc', i') -> Loc.equal loc loc' && i = i'
    | UIDENT (loc, id), UIDENT (loc', id') -> Loc.equal loc loc' && id = id'
    | LIDENT (loc, id), LIDENT (loc', id') -> Loc.equal loc loc' && id = id'
    | _ -> false

  let loc = function
    | EOF loc
    | LPAREN loc | RPAREN loc | COLON loc | ARROW loc | BIND loc | COMMA loc
    | LET loc | REC loc | AND loc | IN loc
    | IF loc | THEN loc | ELSE loc
    | ADD loc | SUB loc | MUL loc | DIV loc | MOD loc
    | LAND loc | LOR loc | LNOT loc
    | EQ loc | NEQ loc
    | LTE loc | LT loc | GT loc | GTE loc
    | BOOL (loc, _)
    | INT (loc, _)
    | UIDENT (loc, _)
    | LIDENT (loc, _) -> loc

  let deloc = function
    | EOF _ -> EOF Loc.dummy
    | LPAREN _ -> LPAREN Loc.dummy
    | RPAREN _ -> RPAREN Loc.dummy
    | COLON _ -> COLON Loc.dummy
    | ARROW _ -> ARROW Loc.dummy
    | BIND _ -> BIND Loc.dummy
    | COMMA _ -> COMMA Loc.dummy
    | LET _ -> LET Loc.dummy
    | REC _ -> REC Loc.dummy
    | AND _ -> AND Loc.dummy
    | IN _ -> IN Loc.dummy
    | IF _ -> IF Loc.dummy
    | THEN _ -> THEN Loc.dummy
    | ELSE _ -> ELSE Loc.dummy
    | ADD _ -> ADD Loc.dummy
    | SUB _ -> SUB Loc.dummy
    | MUL _ -> MUL Loc.dummy
    | DIV _ -> DIV Loc.dummy
    | MOD _ -> MOD Loc.dummy
    | LAND _ -> LAND Loc.dummy
    | LOR _ -> LOR Loc.dummy
    | LNOT _ -> LNOT Loc.dummy
    | EQ _ -> EQ Loc.dummy
    | NEQ _ -> NEQ Loc.dummy
    | LTE _ -> LTE Loc.dummy
    | LT _ -> LT Loc.dummy
    | GT _ -> GT Loc.dummy
    | GTE _ -> GTE Loc.dummy
    | BOOL (_, b) -> BOOL (Loc.dummy, b)
    | INT (_, i) -> INT (Loc.dummy, i)
    | UIDENT (_, id) -> UIDENT (Loc.dummy, id)
    | LIDENT (_, id) -> LIDENT (Loc.dummy, id)

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
