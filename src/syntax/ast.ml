open Common

(* Syntax *)

type expr =
  | Unit of Loc.t
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Float of Loc.t * float
  | String of Loc.t * int * string
  | Blob of Loc.t * int * bytes
  | Timestamp of Loc.t * string
  | Duration of Loc.t * string
  | Tuple of Loc.t * expr list
  | Record of Loc.t * Sym.sym option * field list
  | Var of Loc.t * Sym.sym
  | UnOp of Loc.t * Op.un * expr
  | BinOp of Loc.t * expr * Op.bin * expr
  | If of Loc.t * expr * expr * expr
  | Case of Loc.t * expr * clause list
  | Let of Loc.t * binding * expr
  | LetRec of Loc.t * binding list * expr
  | Abs of Loc.t * param list * Type.t option * expr
  | App of Loc.t * expr * expr list
and field = Loc.t * Sym.sym * expr
and param = Loc.t * Patt.t * Type.t
and binding = Loc.t * Patt.t * Type.t option * expr
and clause = Loc.t * Patt.t * expr

type top =
  | Val of Loc.t * binding
  | Def of Loc.t * binding
  | Type of Loc.t * Sym.sym * Type.t

type name = Loc.t * string
type version = Loc.t * int
type from = Loc.t * (name * version) option
type alias = Loc.t * name * name option
type pkgs = Loc.t * alias list
type import = Loc.t * from option * pkgs

type pkg = Loc.t * name

type file = pkg * import list * top list

(* Constructors *)

let unit loc = Unit loc
let bool loc b = Bool (loc, b)
let int loc i = Int (loc, i)
let float loc f = Float (loc, f)
let string loc len s = String (loc, len, s)
let blob loc len bs = Blob (loc, len, bs)
let timestamp loc ts = Timestamp (loc, ts)
let duration loc d = Duration (loc, d)
let tuple loc exprs = Tuple (loc, exprs)
let record loc constr fields = Record (loc, constr, fields)
let var loc id = Var (loc, id)
let un_op loc op r = UnOp (loc, op, r)
let bin_op loc l op r = BinOp (loc, l, op, r)
let cond loc c t f = If (loc, c, t, f)
let case loc scrut clauses = Case (loc, scrut, clauses)
let bind loc b rest = Let (loc, b, rest)
let bind_rec loc bs rest = LetRec (loc, bs, rest)
let abs loc params res expr = Abs (loc, params, res, expr)
let app loc f xs = App (loc, f, xs)

let field loc id expr = (loc, id, expr)
let param loc patt ty = (loc, patt, ty)
let binding loc patt ty expr = (loc, patt, ty, expr)
let clause loc patt expr = (loc, patt, expr)

let top_val loc b = Val (loc, b)
let top_def loc b = Def (loc, b)
let top_type loc id ty = Type (loc, id, ty)

let name loc id = (loc, id)
let version loc v = (loc, v)
let from loc src = (loc, src)
let alias loc name local = (loc, name, local)
let pkgs loc aliases = (loc, aliases)
let import loc from pkgs = (loc, from, pkgs)

let pkg loc name = (loc, name)

let file pkg imports tops = (pkg, imports, tops)

(* Operations *)

let precedence = function
  | Unit _ | Bool _ | Int _ | Float _ | String _ | Blob _ | Timestamp _ | Duration _
  | Tuple _ | Record _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ | Case _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let loc_expr = function
  | Unit loc
  | Bool (loc, _)
  | Int (loc, _)
  | Float (loc, _)
  | String (loc, _, _)
  | Blob (loc, _, _)
  | Timestamp (loc, _)
  | Duration (loc, _)
  | Tuple (loc, _)
  | Record (loc, _, _)
  | UnOp (loc, _, _)
  | BinOp (loc, _, _, _)
  | If (loc, _, _, _)
  | Case (loc, _, _)
  | Let (loc, _, _)
  | LetRec (loc, _, _)
  | Abs (loc, _, _, _)
  | App (loc, _, _)
  | Var (loc, _) -> loc

let loc_top = function
  | Val (loc, _)
  | Def (loc, _)
  | Type (loc, _, _) -> loc
