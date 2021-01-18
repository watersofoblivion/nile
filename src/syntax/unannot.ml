open Format
open Common

(* Syntax *)

type expr =
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Var of Loc.t * string
  | UnOp of Loc.t * Op.un * expr
  | BinOp of Loc.t * expr * Op.bin * expr
  | If of Loc.t * expr * expr * expr
  | Let of Loc.t * binding * expr
  | LetRec of Loc.t * binding list * expr
  | Abs of Loc.t * string * Type.t * Type.t option * expr
  | App of Loc.t * expr * expr
and binding = Loc.t * string * Type.t option * expr

type top =
  | TopLet of Loc.t * binding
  | TopRec of Loc.t * binding list

type file = top list

(* Constructors *)

let bool loc b = Bool (loc, b)
let int loc i = Int (loc, i)
let var loc id = Var (loc, id)
let un_op loc op r = UnOp (loc, op, r)
let bin_op loc l op r = BinOp (loc, l, op, r)
let bind loc b rest = Let (loc, b, rest)
let bind_rec loc bs rest = LetRec (loc, bs, rest)
let abs loc id ty res expr = Abs (loc, id, ty, res, expr)
let app loc f xs = App (loc, f, xs)
let cond loc c t f = If (loc, c, t, f)
let binding loc id ty expr = (loc, id, ty, expr)

let top_bind loc b = TopLet (loc, b)
let top_bind_rec loc bs = TopRec (loc, bs)

let file tops = tops

(* Operations *)

let precedence = function
  | Bool _ | Int _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let loc_expr = function
  | Bool (loc, _)
  | Int (loc, _)
  | UnOp (loc, _, _)
  | BinOp (loc, _, _, _)
  | Let (loc, _, _)
  | LetRec (loc, _, _)
  | Abs (loc, _, _, _, _)
  | App (loc, _, _)
  | Var (loc, _)
  | If (loc, _, _, _) -> loc

let loc_top = function
  | TopLet (loc, _)
  | TopRec (loc, _) -> loc

(* Pretty Printing *)

let rec pp_expr expr fmt = match expr with
  | Bool (_, b) -> fprintf fmt "%b" b
  | Int (_, i) -> fprintf fmt "%d" i
  | Var (_, id) -> fprintf fmt "%s" id
  | UnOp (_, op, r) ->
    let prec = Op.un_precedence op in
    fprintf fmt "%t%t" (Op.pp_un op) (print_precedence prec r);
  | BinOp (_, l, op, r) ->
    let prec = Op.bin_precedence op in
    fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence prec l) (Op.pp_bin op) (print_precedence prec r)
  | Let (_, b, rest) -> fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding b) (pp_expr rest)
  | LetRec (_, bs, rest) -> fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings bs) (pp_expr rest)
  | Abs (_, ps, ty, expr) -> fprintf fmt "(%t): %t => %t" (pp_params ps) (Type.pp ty) (pp_expr expr)
  | App (_, f, xs) -> fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence 0 f) (pp_args xs)
  | If (_, c, t, f) -> fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp_expr c) (pp_expr t) (pp_expr f)
and print_precedence prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp_expr expr)
  else fprintf fmt "%t" (pp_expr expr)
and pp_bindings bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding b) in
  pp_print_list ~pp_sep pp_b fmt bs
and pp_binding (_, id, ty, expr) fmt = match expr with
  | Abs (_, ps, ty, expr) ->
    fprintf fmt "%s(" id;
    pp_params ps fmt;
    fprintf fmt "): %t =@;<1 2>%t" (Type.pp ty) (pp_expr expr)
  | expr -> fprintf fmt "%s: %t =@;<1 2>%t" id (Type.pp ty) (pp_expr expr)
and pp_params ps fmt =
  let pp_sep fmt _ = fprintf fmt ", " in
  let param fmt (_, id, ty) = fprintf fmt "%s: %t" id (Type.pp ty) in
  pp_print_list ~pp_sep param fmt ps
and pp_args xs fmt =
  let pp_sep fmt _ = fprintf fmt "@ " in
  let pp_arg fmt arg = print_precedence 0 arg fmt in
  pp_print_list ~pp_sep pp_arg fmt xs

let pp_top top fmt = match top with
  | TopLet (_, b) -> fprintf fmt "@[<hv>let %t@]" (pp_binding b)
  | TopRec (_, bs) -> fprintf fmt "@[<hv>let rec %t" (pp_bindings bs)

let pp_file file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"
