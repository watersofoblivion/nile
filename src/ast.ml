open Format

type t =
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | UnOp of Loc.t * Op.un * t
  | BinOp of Loc.t * t * Op.bin * t
  | Let of Loc.t * b * t
  | LetRec of Loc.t * b list * t
  | Abs of Loc.t * p list * Type.t * t
  | App of Loc.t * t * t list
  | Var of Loc.t * string
  | If of Loc.t * t * t * t
and b = Loc.t * string * Type.t * t
and p = Loc.t * string * Type.t

let bool ?loc:(loc = Loc.dummy) b = Bool (loc, b)
let int ?loc:(loc = Loc.dummy) i = Int (loc, i)
let un_op ?loc:(loc = Loc.dummy) op r = UnOp (loc, op, r)
let bin_op ?loc:(loc = Loc.dummy) l op r = BinOp (loc, l, op, r)
let bind ?loc:(loc = Loc.dummy) b rest = Let (loc, b, rest)
let bind_rec ?loc:(loc = Loc.dummy) bs rest = LetRec (loc, bs, rest)
let abs ?loc:(loc = Loc.dummy) ps ty expr = Abs (loc, ps, ty, expr)
let app ?loc:(loc = Loc.dummy) f xs = App (loc, f, xs)
let var ?loc:(loc = Loc.dummy) id = Var (loc, id)
let cond ?loc:(loc = Loc.dummy) c t f = If (loc, c, t, f)
let binding ?loc:(id_loc = Loc.dummy) id ty expr = (id_loc, id, ty, expr)
let param ?loc:(id_loc = Loc.dummy) id ty = (id_loc, id, ty)

let precedence = function
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | _ -> 0

let rec pp ast fmt = match ast with
  | Bool (_, b) -> fprintf fmt "%b" b
  | Int (_, i) -> fprintf fmt "%d" i
  | UnOp (_, op, r) ->
    let prec = Op.un_precedence op in
    fprintf fmt "%t%t" (Op.pp_un op) (print_precedence prec r);
  | BinOp (_, l, op, r) ->
    let prec = Op.bin_precedence op in
    fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence prec l) (Op.pp_bin op) (print_precedence prec r)
  | Let (_, b, rest) -> fprintf fmt "let %t in %t" (pp_binding b) (pp rest)
  | LetRec (_, bs, rest) -> fprintf fmt "let rec %t in %t" (pp_bindings bs) (pp rest)
  | Abs (_, ps, ty, expr) -> fprintf fmt "(%t): %t -> %t" (pp_params ps) (Type.pp ty) (pp expr)
  | App (_, (Abs _ as f), xs) -> fprintf fmt "(%t) %t" (pp f) (pp_args xs)
  | App (_, f, xs) -> fprintf fmt "%t %t" (pp f) (pp_args xs)
  | Var (_, id) -> fprintf fmt "%s" id
  | If (_, c, t, f) -> fprintf fmt "@[<hv>if %t@ then %t@ else %t@]" (pp c) (pp t) (pp f)
and print_precedence op_prec expr fmt =
  if op_prec < precedence expr
  then fprintf fmt "(%t)" (pp expr)
  else fprintf fmt "%t" (pp expr)
and pp_bindings bs fmt =
  let pp_sep fmt _ = fprintf fmt " and " in
  let pp_b fmt b = pp_binding b fmt in
  pp_print_list ~pp_sep pp_b fmt bs
and pp_binding (_, id, ty, expr) fmt = fprintf fmt "%s: %t = %t" id (Type.pp ty) (pp expr)
and pp_params ps fmt =
  let pp_sep fmt _ = fprintf fmt ", " in
  let param fmt (_, id, ty) = fprintf fmt "%s: %t" id (Type.pp ty) in
  pp_print_list ~pp_sep param fmt ps
and pp_args xs fmt =
  let pp_sep fmt _ = fprintf fmt " " in
  let pp_x fmt x = pp x fmt in
  pp_print_list ~pp_sep pp_x fmt xs

let rec equal ast ast' = match ast, ast' with
  | Bool (_, b), Bool (_, b') -> b = b'
  | Int (_, i), Int (_, i') -> i = i'
  | UnOp (_, op, r), UnOp (_, op', r') -> Op.un_equal op op' && equal r r'
  | BinOp (_, l, op, r), BinOp (_, l', op', r') -> equal l l' && Op.bin_equal op op' && equal r r'
  | Let (_, b, rest), Let (_, b', rest') -> binding_equal b b' && equal rest rest'
  | LetRec (_, bs, rest), LetRec (_, bs', rest') ->
    let for_all (b, b') = binding_equal b b' in
    List.combine bs bs'
      |> List.for_all for_all
      |> (&&) (equal rest rest')
  | Abs (_, ps, ty, expr), Abs (_, ps', ty', expr') ->
    let for_all (p, p') = param_equal p p' in
    List.combine ps ps'
      |> List.for_all for_all
      && Type.equal ty ty'
      && equal expr expr'
  | App (_, f, xs), App (_, f', xs') ->
    let for_all (x, x') = equal x x' in
    equal f f' &&
    List.combine xs xs'
      |> List.for_all for_all
  | Var (_, id), Var (_, id') -> id = id'
  | If (_, c, t, f), If (_, c', t', f') -> equal c c' && equal t t' && equal f f'
  | _ -> false
and binding_equal (_, id, ty, expr) (_, id', ty', expr') = id = id' && Type.equal ty ty' && equal expr expr'
and param_equal (_, id, ty) (_, id', ty') = id = id' && Type.equal ty ty'

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

let rec deloc = function
  | Bool (_, b) -> Bool (Loc.dummy, b)
  | Int (_, i) -> Int (Loc.dummy, i)
  | UnOp (_, op, r) -> UnOp (Loc.dummy, op, deloc r)
  | BinOp (_, l, op, r) -> BinOp (Loc.dummy, deloc l, op, deloc r)
  | Let (_, b, rest) -> Let (Loc.dummy, deloc_binding b, deloc rest)
  | LetRec (_, bs, rest) -> LetRec (Loc.dummy, List.map deloc_binding bs, deloc rest)
  | Abs (_, ps, ty, expr) -> Abs (Loc.dummy, List.map deloc_param ps, ty, deloc expr)
  | App (_, f, xs) -> App (Loc.dummy, deloc f, List.map deloc xs)
  | Var (_, id) -> Var (Loc.dummy, id)
  | If (_, c, t, f) -> If (Loc.dummy, deloc c, deloc t, deloc f)
and deloc_binding (_, id, ty, expr) = (Loc.dummy, id, ty, expr)
and deloc_param (_, id, ty) = (Loc.dummy, id, ty)
