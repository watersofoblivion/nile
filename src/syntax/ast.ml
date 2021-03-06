open Format
open Common

(* Syntax *)

type expr =
  | Unit of Loc.t
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Float of Loc.t * float
  | String of Loc.t * int * string
  | Tuple of Loc.t * expr list
  | Record of Loc.t * Sym.sym * field list
  | Var of Loc.t * Sym.sym
  | UnOp of Loc.t * Op.un * expr
  | BinOp of Loc.t * expr * Op.bin * expr
  | If of Loc.t * expr * expr * expr
  | Case of Loc.t * expr * clause list
  | Let of Loc.t * binding * expr
  | LetRec of Loc.t * binding list * expr
  | Abs of Loc.t * param list * Type.t option * expr
  | App of Loc.t * expr list * expr
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

let top_val loc b = TopVal (loc, b)
let top_def loc b = TopDef (loc, b)
let top_type loc id ty = TopDef (loc, id, b)

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
  | Unit _ | Bool _ | Int _ | Float _ | String _ | Tuple _ | Record _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _, Case _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let loc_expr = function
  | Unit loc
  | Bool (loc, _)
  | Int (loc, _)
  | Float (loc, _)
  | String (loc, _, _)
  | Tuple (loc, _)
  | Record (loc, _, _)
  | UnOp (loc, _, _)
  | BinOp (loc, _, _, _)
  | If (loc, _, _, _)
  | Case (loc, _, _)
  | Let (loc, _, _)
  | LetRec (loc, _, _)
  | Abs (loc, _, _, _, _)
  | App (loc, _, _)
  | Var (loc, _) -> loc

let loc_top = function
  | TopVal (loc, _)
  | TopDef (loc, _)
  | TopType (loc, _) -> loc

(* Pretty Printing *)

let rec pp_expr names expr fmt = match expr with
  | Unit _ -> pp_unit fmt
  | Bool (_, b) -> pp_bool b fmt
  | Int (_, i) -> pp_int i fmt
  | Float (_, f) -> pp_float f fmt
  | String (_, _, s) -> pp_string s fmt
  | Tuple (_, exprs) -> pp_tuple names exprs fmt
  | Record (_, constr, fields) -> pp_record names constr fields fmt
  | Var (_, id) -> pp_var names id fmt
  | UnOp (_, op, r) -> pp_un_op names op r fmt
  | BinOp (_, l, op, r) -> pp_bin_op names l op r fmt
  | If (_, c, t, f) -> pp_cond names c t f fmt
  | Case (_, scrut, clauses) -> pp_case names scrut clauses fmt
  | Let (_, b, rest) -> pp_bind names b rest fmt
  | LetRec (_, bs, rest) -> pp_bind_rec names bs rest fmt
  | Abs (_, params, res, expr) -> pp_abs names params res expr fmt
  | App (_, f, xs) -> pp_app names f xs fmt

and pp_unit fmt =
  fprintf fmt "()"

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_float f fmt =
  fprintf fmt "%f" f

and pp_string s fmt =
  fprintf fmt "%S" s

and pp_tuple names exprs fmt =
  fprintf fmt "(@[<hv>";
  let pp_e fmt expr = pp_expr names expr fmt in
  let pp_sep fmt _ = fprintf fmt ",@ " in
  pp_print_list ~pp_sep pp_e exprs fmt;
  fprintf fmt "@])"

and pp_record names constr fields fmt =
  let constr = Sym.name_of constr names in
  fprintf fmt "%s{@[<hv>" constr;
  let pp_field fmt (_, name, expr) =
    let name = Sym.name_of name in
    fprintf fmt "@[<hv>%s:@ %t@]" name (pp_expr names expr)
  in
  let pp_sep fmt _ = fprintf fmt ",@ " in
  pp_print_list ~pp_sep pp_field fields fmt;
  fprintf fmt "@]}"

and pp_var names id fmt =
  Sym.name_of id names
    |> fprintf fmt "%s"

and pp_un_op names op r fmt =
  let prec = Op.un_precedence op in
  fprintf fmt "%t%t" (Op.pp_un op) (print_precedence names prec r);

and pp_bin_op names l op r fmt =
  let prec = Op.bin_precedence op in
  fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence names prec l) (Op.pp_bin op) (print_precedence names prec r)

and pp_cond names c t f fmt =
  fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp_expr names c) (pp_expr names t) (pp_expr names f)

and pp_case names scrut clauses fmt =

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding names b) (pp_expr names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings names bs) (pp_expr names rest)

and pp_abs names patt ty res expr fmt =
  let id = Sym.name_of id names in
  fprintf fmt "(%t: %t" (Patt.pp names patt) (Type.pp ty);
  let res = pp_params names res expr fmt in
  let _ = match res with
    | Some res -> fprintf fmt "): %t" (Type.pp res)
    | None -> fprintf fmt ")"
  in
  fprintf fmt " => %t" (pp_expr names expr)

and pp_app names f x fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence names 0 f) (pp_expr names x)

and print_precedence names prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp_expr names expr)
  else fprintf fmt "%t" (pp_expr names expr)

and pp_bindings names bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding names b) in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding names (_, patt, ty, expr) fmt =
  match expr with
    | Abs (_, patt', ty, res, expr) ->
      fprintf fmt "%s(%s: %t" (Patt.pp names patt) (Patt.pp names patt') (Type.pp ty);
      let res = pp_params names res expr fmt in
      fprintf fmt ")";
      let _ = match res with
        | Some res -> fprintf fmt ": %t" (Type.pp res)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr names expr)
    | expr ->
      fprintf fmt "%t" (Patt.pp names patt);
      let _ = match ty with
        | Some ty -> fprintf fmt ": %t" (Type.pp ty)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr names expr)

and pp_params names res expr fmt = match expr with
  | Abs (_, patt, ty, res, expr) ->
    fprintf fmt ", %t: %t" (Patt.pp names patt) (Type.pp ty);
    pp_params names res expr fmt
  | _ -> res

and pp_clause names patt expr fmt =

let pp_top_val names b fmt =
  fprintf fmt "@[<hv>let %t@]" (pp_binding names b)

let pp_top_def names b fmt =
  fprintf fmt "@[<hv>let rec %t" (pp_bindings names bs)

let pp_top_ty names id ty fmt =

let pp_top names top fmt = match top with
  | TopVal (_, b) -> pp_top_val names b fmt
  | TopDef (_, b) -> pp_top_def names b fmt
  | TopType (_, id, ty) -> pp_top_ty names id ty fmt

let pp_name names (_, id) fmt =
  fprintf fmt "%s" id

let pp_version names (_, v) fmt =
  fprintf fmt "%d" v

let pp_from names (_, src) fmt = match src with
  | Some (name, version) -> fprintf fmt "%t@v%t" (pp_name names name) (pp_version names version)
  | None -> fprintf fmt "_"

let pp_alias names (_, name, local) fmt = match local with
  | Some local -> fprintf fmt "%t -> \"%t\"" (pp_name names local) (pp_name names name)
  | None -> fprintf fmt "\"%t\"" (pp_name names name)

let pp_pkgs names (_, aliases) fmt =
  let pp_sep fmt _ = fprintf fmt "@ " in
  let pp_alias fmt alias = fprintf fmt "| %t" (pp_alias names alias) in
  fprintf fmt "@[v";
  pp_print_list ~pp_sep pp_alias aliases fmt;
  fprintf fmt "@]"

let pp_import names (_, from, pkgs) fmt =
  fprintf fmt "@[v";
  let _ =
    match from with
      | Some from ->
        pp_from names from fmt;
        fprintf fmt " "
      | None -> ()
  in
  fprintf fmt "import@ ";
  pp_pkgs names pkgs fmt;
  fprintf fmt "@]"

let pp_pkg names (_, id) fmt =
  fprintf fmt "package %t" (pp_name names id)

let pp_file names (pkg, imports, tops) fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top names top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"
