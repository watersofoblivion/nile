open Common

type t = {
  loc:        Loc.t;
  precedence: int;
  lexeme:     string
}

let op loc precedence lexeme = { op; precedence; lexeme }

let loc op = op.loc
let precedence op = op.precedence
