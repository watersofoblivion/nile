open Format
open Common

(* Syntax *)

type atom =
  | Bool of bool
  | Int of int
  | Var of Sym.s
  | Abs of Sym.s * Type.t * Type.t * block
and expr =
  | UnOp of Op.un * atom
  | BinOp of atom * Op.bin * atom
  | App of atom * atom
  | Atom of atom
and block =
  | Let of binding * block
  | LetRec of binding list * block
  | If of atom * block * block
  | Expr of expr
and binding = Sym.s * Type.t * expr

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let bool b = Bool b
let int i = Int i
let var sym = Var sym
let abs sym arg res body = Abs (sym, arg, res, body)

let un_op op r = UnOp (op, r)
let bin_op l op r = BinOp (l, op, r)
let app f x = App (f, x)
let atom a = Atom a

let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let cond c t f = If (c, t, f)
let expr c = Expr c

let binding sym ty expr = (sym, ty, expr)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops

(* Pretty-Printing *)

let rec pp_atom tbl atom fmt = match atom with
  | Bool b -> pp_bool b fmt
  | Int i -> pp_int i fmt
  | Var sym -> pp_var tbl sym fmt
  | Abs (id, arg, res, body) -> pp_abs tbl id arg res body fmt

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_var tbl sym fmt =
  Sym.name_of sym tbl
    |> fprintf fmt "%s"

and pp_abs tbl sym arg res body fmt =
  let id = Sym.name_of sym tbl in
  fprintf fmt "@[<hv>(%s: %t): %t =>@;<1 2>%t@]" id (Type.pp arg) (Type.pp res) (pp_block tbl body)

and pp_expr tbl expr fmt = match expr with
  | UnOp (op, r) -> pp_un_op tbl op r fmt
  | BinOp (l, op, r) -> pp_bin_op tbl l op r fmt
  | App (f, x) -> pp_app tbl f x fmt
  | Atom atom -> pp_atom tbl atom fmt

and pp_un_op tbl op r fmt =
  fprintf fmt "%t%t" (Op.pp_un op) (pp_atom tbl r)

and pp_bin_op tbl l op r fmt =
  fprintf fmt "%t %t %t" (pp_atom tbl l) (Op.pp_bin op) (pp_atom tbl r)

and pp_app tbl f x fmt =
  fprintf fmt "%t %t" (pp_atom tbl f) (pp_atom tbl x)

and pp_block tbl block fmt = match block with
  | Let (b, rest) -> pp_bind tbl b rest fmt
  | LetRec (bs, rest) -> pp_bind_rec tbl bs rest fmt
  | If (c, t, f) ->  pp_cond tbl c t f fmt
  | Expr expr -> pp_expr tbl expr fmt

and pp_bind tbl b rest fmt =
  fprintf fmt "@[<v>let %t in@ %t@]" (pp_binding tbl b) (pp_block tbl rest)

and pp_bind_rec tbl bs rest fmt =
  fprintf fmt "@[<v>let rec %t in@ %t@]" (pp_bindings tbl bs) (pp_block tbl rest)

and pp_cond tbl c t f fmt =
  fprintf fmt "@[<v>if %t@ then %t@ else %t@]" (pp_atom tbl c) (pp_block tbl t) (pp_block tbl f)

and pp_bindings tbl bs fmt =
  let pp_sep fmt _ = fprintf fmt " " in
  let pp_b b fmt = pp_binding tbl fmt b in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding tbl (sym, ty, e) fmt =
  let id = Sym.name_of sym tbl in
  fprintf fmt "%s: %t = %t" id (Type.pp ty) (pp_expr tbl e)

let pp_top tbl top fmt = match top with
  | TopLet b -> fprintf fmt "@[<v>let %t@]" (pp_binding tbl b)
  | TopRec bs -> fprintf fmt "@[<v>let rec %t" (pp_bindings tbl bs)

let pp_file tbl file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top tbl top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"

(* Type Checking *)

let (builtin_idx, builtin, builtin_aenv, builtin_tenv) =
  let fold (idx, env, aenv, tenv) (id, ty) =
    let env = Check.bind idx ty env in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    (idx + 1, env, aenv, tenv)
  in
  List.fold_left fold (0, Check.env, [], []) Builtin.builtins

let rec type_of_atom env = function
  | Bool _ -> Type.bool
  | Int _ -> Type.int
  | Var sym -> type_of_var env sym
  | Abs (sym, ty, res, body) -> type_of_abs env sym ty res body

and type_of_var env sym =
  try Check.lookup sym env
  with Not_found -> Check.unbound_identifier sym

and type_of_abs env sym ty res body =
  let env = Check.bind sym ty env in
  let res' = type_of_block env body in
  if Type.equal res res'
  then res
  else Check.declaration_mismatch sym res res'

and type_of_expr env = function
  | UnOp (op, r) -> type_of_un_op env op r
  | BinOp (l, op, r) -> type_of_bin_op env l op r
  | App (f, x) -> type_of_app env f x
  | Atom atom -> type_of_atom env atom

and type_of_un_op env op r =
  let r = type_of_atom env r in
  Op.type_of_un op r

and type_of_bin_op env l op r =
  let l = type_of_atom env l in
  let r = type_of_atom env r in
  Op.type_of_bin l op r

and type_of_app env f x =
  let x = type_of_atom env x in
  match type_of_atom env f with
    | Type.Fun (arg, res) ->
      if Type.equal arg x
      then res
      else Check.invalid_args arg x
    | ty -> Check.cannot_apply ty

and type_of_block env = function
  | Let ((sym, ty, expr), rest) -> type_of_bind env sym ty expr rest
  | LetRec (bs, rest) -> type_of_bind_rec env bs rest
  | If (c, t, f) -> type_of_cond env c t f
  | Expr expr -> type_of_expr env expr

and type_of_bind env sym ty expr rest =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then
    let env = Check.bind sym ty env in
    type_of_block env rest
  else Check.declaration_mismatch sym ty expr

and type_of_bind_rec env bs rest =
  let fold env (sym, ty, _) = Check.bind sym ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (sym, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Check.declaration_mismatch sym ty expr
    in
    List.iter iter bs
  in
  type_of_block env rest

and type_of_cond env c t f = match type_of_atom env c with
  | Type.Bool ->
    let t = type_of_block env t in
    let f = type_of_block env f in
    if Type.equal t f
    then t
    else Check.conditional_branch_mismatch t f
  | ty -> Check.invalid_condition ty

let type_of_top_bind env sym ty expr =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then Check.bind sym ty env
  else Check.declaration_mismatch sym ty expr

let type_of_top_bind_rec env bs =
  let fold env (sym, ty, _) = Check.bind sym ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (sym, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Check.declaration_mismatch sym ty expr
    in
    List.iter iter bs
  in
  env

let type_of_top env = function
  | TopLet (sym, ty, expr) -> type_of_top_bind env sym ty expr
  | TopRec bs -> type_of_top_bind_rec env bs

let type_of_file = List.fold_left type_of_top
