open Format

type t =
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Var of Loc.t * string
  | UnOp of Loc.t * Op.un * t
  | BinOp of Loc.t * t * Op.bin * t
  | If of Loc.t * t * t * t
  | Let of Loc.t * b * t
  | LetRec of Loc.t * b list * t
  | Abs of Loc.t * p list * Type.t * t
  | App of Loc.t * t * t list
and b = Loc.t * string * Type.t * t
and p = Loc.t * string * Type.t

let bool loc b = Bool (loc, b)
let int loc i = Int (loc, i)
let var loc id = Var (loc, id)
let un_op loc op r = UnOp (loc, op, r)
let bin_op loc l op r = BinOp (loc, l, op, r)
let bind loc b rest = Let (loc, b, rest)
let bind_rec loc bs rest = LetRec (loc, bs, rest)
let abs loc ps ty expr = Abs (loc, ps, ty, expr)
let app loc f xs = App (loc, f, xs)
let cond loc c t f = If (loc, c, t, f)
let binding loc id ty expr = (loc, id, ty, expr)
let param loc id ty = (loc, id, ty)

let precedence = function
  | Bool _ | Int _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let rec pp ast fmt = match ast with
  | Bool (_, b) -> fprintf fmt "%b" b
  | Int (_, i) -> fprintf fmt "%d" i
  | Var (_, id) -> fprintf fmt "%s" id
  | UnOp (_, op, r) ->
    let prec = Op.un_precedence op in
    fprintf fmt "%t%t" (Op.pp_un op) (print_precedence prec r);
  | BinOp (_, l, op, r) ->
    let prec = Op.bin_precedence op in
    fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence prec l) (Op.pp_bin op) (print_precedence prec r)
  | Let (_, b, rest) -> fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding b) (pp rest)
  | LetRec (_, bs, rest) -> fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings bs) (pp rest)
  | Abs (_, ps, ty, expr) -> fprintf fmt "(%t): %t => %t" (pp_params ps) (Type.pp ty) (pp expr)
  | App (_, f, xs) -> fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence 0 f) (pp_args xs)
  | If (_, c, t, f) -> fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp c) (pp t) (pp f)
and print_precedence prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp expr)
  else fprintf fmt "%t" (pp expr)
and pp_bindings bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding b) in
  pp_print_list ~pp_sep pp_b fmt bs
and pp_binding (_, id, ty, expr) fmt = match expr with
  | Abs (_, ps, ty, expr) ->
    fprintf fmt "%s(" id;
    pp_params ps fmt;
    fprintf fmt "): %t =@;<1 2>%t" (Type.pp ty) (pp expr)
  | expr -> fprintf fmt "%s: %t =@;<1 2>%t" id (Type.pp ty) (pp expr)
and pp_params ps fmt =
  let pp_sep fmt _ = fprintf fmt ", " in
  let param fmt (_, id, ty) = fprintf fmt "%s: %t" id (Type.pp ty) in
  pp_print_list ~pp_sep param fmt ps
and pp_args xs fmt =
  let pp_sep fmt _ = fprintf fmt "@ " in
  let pp_arg fmt arg = print_precedence 0 arg fmt in
  pp_print_list ~pp_sep pp_arg fmt xs

let loc = function
  | Bool (loc, _)
  | Int (loc, _)
  | UnOp (loc, _, _)
  | BinOp (loc, _, _, _)
  | Let (loc, _, _)
  | LetRec (loc, _, _)
  | Abs (loc, _, _, _)
  | App (loc, _, _)
  | Var (loc, _)
  | If (loc, _, _, _) -> loc
