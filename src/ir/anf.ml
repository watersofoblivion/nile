open Format
open Common


(* Syntax *)

type atom = private
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Byte of bytes
  | Blob of bytes
  | Timestamp of string
  | Duration of string
  | Var of Sym.sym
  | Abs of param list * Type.t * block
  | Join of param list * Type.t * block
and param = Patt.t * Type.t
and expr = private
  | App of atom * atom list
  | Tail of atom * atom list
  | Jump of atom * atom list
  | Builtin of Sym.sym * atom list
  | Tuple of atom list
  | Proj of atom * int
  | Constr of Sym.sym * atom option
  | Atom of atom
and block = private
  | Let of binding * block
  | LetRec of binding list * block
  | Case of atom * clause list * Type.t
  | Expr of expr
and binding = Patt.t * Type.t * expr
and clause = Patt.t * block

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let unit = Unit
let bool b = Bool { b }
let int i = Int { i }
let float f = Float { f }
let rune r = Rune { r }
let string s = String { length = String.length s; s }
let byte b = Byte { b }
let blob bs = Blob { length = Bytes.length bs; bs }
let timestamp ts = Timestamp { ts }
let duration d = Duration { d }
let var id = Var { id }
let abs ?join:(join = false) params res body = Abs { arity = List.length params; join; params; res; body }

let param name ty = Param { name; ty }

let app ?tail:(tail = false) ?jump:(jump = false) f args = App { arity = List.length args; tail = tail || jump; jump; fn; args }
let builtin builtin args = Builtin { builtin; args }
let tuple exprs = Tuple { arity = List.length exprs; exprs }
let proj tuple index = Proj { tuple; index }
let constr name args ty = Constr { name; args; ty }
let atom a = Atom { a }

let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let case scrut clauses res = Case (scrut, clauses, res)
let expr c = Expr c

let binding patt ty expr = (patt, ty, expr)
let clause patt body = (patt, body)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops
