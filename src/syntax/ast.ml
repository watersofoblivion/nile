open Common

(* Syntax *)

type expr =
  | Unit of { loc: Loc.t }
  | Bool of { loc: Loc.t; lexeme: string }
  | Int of { loc: Loc.t; lexeme: string; radix: int }
  | Float of { loc: Loc.t; lexeme: string; hex: bool }
  | Rune of { loc: Loc.t; lexeme: string }
  | String of { loc: Loc.t; lexeme: string }
  | Byte of { loc: Loc.t; lexeme: string }
  | Blob of { loc: Loc.t; lexeme: string }
  | Timestamp of { loc: Loc.t; lexeme: string }
  | Duration of { loc: Loc.t; lexeme: string }
  | Tuple of { loc: Loc.t; exprs: expr list }
  | Record of { loc: Loc.t; constr: Sym.sym option; fields: field list }
  | Var of { loc: Loc.t; id: Sym.sym }
  | UnOp of { loc: Loc.t; op: Op.t; prec: int; rhs: expr }
  | BinOp of { loc: Loc.t; op: Op.t; prec: int; lhs: expr; rhs: expr }
  | Slice of { loc: Loc.t; expr: t; start: t option; stop: t option }
  | Index of { loc: Loc.t; expr: t; idx: t }
  | If of { loc: Loc.t; cond: expr; tru: expr; fls: expr }
  | Case of { loc: Loc.t; scrut: expr; clauses: clause list }
  | Let of { loc: Loc.t; binding: binding; scope: expr }
  | LetRec of { loc: Loc.t; bindings: binding list; scope: expr }
  | Abs of { loc: Loc.t; params: param list; res: Type.t option; body: expr }
  | App of { loc: Loc.t; fn: expr; args: expr list }
and field = Field of { loc: Loc.t; name: Sym.sym; value: expr }
and param = Param of { loc: Loc.t; name: Patt.t; ty: Type.t }
and binding = Binding of { loc: Loc.t; name: Patt.t; ty: Type.t option; body: expr }
and clause = Clause of { loc: Loc.t; patt: Patt.t; body: expr }

type top =
  | Val of { loc: Loc.t; binding: binding }
  | Def of { loc: Loc.t; binding: binding }
  | Type of { loc: Loc.t; name: Sym.sym; defn: Type.t }

type name = Name of { loc: Loc.t; name: Sym.sym }
type version = Version of { loc: Loc.t; version: int }
type src = Source of { loc: Loc.t; mojule: name; version: version }
type from = From of { loc: Loc.t; source: src option }
type alias = Alias of { loc: Loc.t; package: name; alias: name option }
type pkgs = Packages of { loc: Loc.t; clauses: alias list }
type import = Import of { loc: Loc.t; from: from option; package: pkgs }

type pkg = Package of { loc: Loc.t; name: name }

type file = File of { package: pkg; imports: import list; tops: top list }

(* Constructors *)

let unit loc = Unit { loc }
let bool loc lexeme = Bool { loc; lexeme }
let int loc lexeme radix = Int { loc; lexeme; radix }
let float loc lexeme hex = Float { loc; lexeme; hex }
let rune loc lexeme = Rune { loc; lexeme }
let string loc lexeme = String { loc; lexeme }
let byte loc lexeme = Byte { loc; lexeme }
let blob loc lexeme = Blob { loc; lexeme }
let timestamp loc lexeme = Timestamp { loc; lexeme }
let duration loc lexeme = Duration { loc; lexeme }
let tuple loc exprs = Tuple { loc; exprs }
let record loc constr fields = Record { loc; constr; fields }
let var loc id = Var { loc; id }
let un_op loc op prec rhs = UnOp { loc; op; prec; r }
let bin_op loc op prec lhs rhs = BinOp { loc; op; prec; lhs; rhs }
let slice loc expr start stop = Slice { loc; expr; start; stop }
let index loc expr idx = Index { loc; expr; idx }
let cond loc cond tru fls = If { loc; cond; tru; fls }
let case loc scrut clauses = Case { loc; scrut; clauses }
let bind loc binding scope = Let { loc; binding; scope }
let bind_rec loc bindings rest = LetRec { loc; bindings; scope }
let abs loc params res body = Abs { loc; params; res; body }
let app loc fn args = App { loc; fn; args }

let field loc name value = Field { loc; name; value }
let param loc name ty = Param { loc; name; ty }
let binding loc name ty expr = Binding { loc; name; ty; expr }
let clause loc patt expr = Clause { loc; patt; expr }

let top_val loc binding = Val { loc; binding }
let top_def loc binding = Def { loc; binding }
let top_type loc name ty = Type { loc; name; ty }

let name loc id = Name { loc; id }
let version loc major = Version { loc; major }
let src loc mojule version = Source { loc; mojule; version }
let from loc src = From { loc; src }
let alias loc package alias = Alias { loc; package; alias }
let pkgs loc clauses = Packages { loc; clauses }
let import loc from packages = Import { loc; from; packages }

let pkg loc name = Package { loc; name }

let file package imports tops = File { package; imports; tops }

(* Operations *)

let precedence = function
  | Unit _ | Bool _ | Int _ | Float _ | Rune _ | String _ | Byte _ | Blob _ | Timestamp _ | Duration _
  | Tuple _ | Record _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ | Case _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let loc_expr = function
  | Unit expr -> expr.loc
  | Bool expr -> expr.loc
  | Int expr -> expr.loc
  | Float expr -> expr.loc
  | Rune expr -> expr.loc
  | String expr -> expr.loc
  | Byte expr -> expr.loc
  | Blob expr -> expr.loc
  | Timestamp expr -> expr.loc
  | Duration expr -> expr.loc
  | Tuple expr -> expr.loc
  | Record expr -> expr.loc
  | UnOp expr -> expr.loc
  | BinOp expr -> expr.loc
  | Slice expr -> expr.loc
  | Index expr -> expr.loc
  | If expr -> expr.loc
  | Case expr -> expr.loc
  | Let expr -> expr.loc
  | LetRec expr -> expr.loc
  | Abs expr -> expr.loc
  | App expr -> expr.loc
  | Var expr -> expr.loc

let loc_top = function
  | Val top -> top.loc
  | Def top -> top.loc
  | Type top -> top.loc
