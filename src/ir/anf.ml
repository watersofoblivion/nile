open Format
open Common

(* Syntax *)

type atom =
  | Unit
  | Bool of bool
  | Int of int
  | Var of Sym.sym
  | Abs of Patt.t * Type.t * Type.t * block
and expr =
  | UnOp of Op.un * atom
  | BinOp of atom * Op.bin * atom
  | App of atom * atom
  | Atom of atom
and block =
  | Let of binding * block
  | LetRec of binding list * block
  | Case of atom * case list
  | Expr of expr
and binding = Patt.t * Type.t * expr
and case = Patt.t * block

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let unit = Unit
let bool b = Bool b
let int i = Int i
let var sym = Var sym
let abs patt arg res body = Abs (patt, arg, res, body)

let un_op op r = UnOp (op, r)
let bin_op l op r = BinOp (l, op, r)
let app f x = App (f, x)
let atom a = Atom a

let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let case_of scrut cases = Case (scrut, cases)
let expr c = Expr c

let binding patt ty expr = (patt, ty, expr)
let case patt body = (patt, body)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops

(* Pretty-Printing *)

let rec pp_atom names atom fmt = match atom with
  | Unit -> pp_unit fmt
  | Bool b -> pp_bool b fmt
  | Int i -> pp_int i fmt
  | Var sym -> pp_var names sym fmt
  | Abs (id, arg, res, body) -> pp_abs names id arg res body fmt

and pp_unit fmt =
  fprintf fmt "()"

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_var names sym fmt =
  Sym.name_of sym names
    |> fprintf fmt "%s"

and pp_abs names patt arg res body fmt =
  fprintf fmt "@[<hv>(%t: %t): %t =>@;<1 2>%t@]" (Patt.pp names patt) (Type.pp arg) (Type.pp res) (pp_block names body)

and pp_expr names expr fmt = match expr with
  | UnOp (op, r) -> pp_un_op names op r fmt
  | BinOp (l, op, r) -> pp_bin_op names l op r fmt
  | App (f, x) -> pp_app names f x fmt
  | Atom atom -> pp_atom names atom fmt

and pp_un_op names op r fmt =
  fprintf fmt "%t%t" (Op.pp_un op) (pp_atom names r)

and pp_bin_op names l op r fmt =
  fprintf fmt "%t %t %t" (pp_atom names l) (Op.pp_bin op) (pp_atom names r)

and pp_app names f x fmt =
  fprintf fmt "%t %t" (pp_atom names f) (pp_atom names x)

and pp_block names block fmt = match block with
  | Let (b, rest) -> pp_bind names b rest fmt
  | LetRec (bs, rest) -> pp_bind_rec names bs rest fmt
  | Case (scrut, cases) ->  pp_case_of names scrut cases fmt
  | Expr expr -> pp_expr names expr fmt

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>let %t in@ %t@]" (pp_binding names b) (pp_block names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>let rec %t in@ %t@]" (pp_bindings names bs) (pp_block names rest)

and pp_case_of names scrut cases fmt =

and pp_bindings names bs fmt =
  let pp_sep fmt _ = fprintf fmt " " in
  let pp_b b fmt = pp_binding names fmt b in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding names (sym, ty, e) fmt =
  let id = Sym.name_of sym names in
  fprintf fmt "%s: %t = %t" id (Type.pp ty) (pp_expr names e)

let pp_top names top fmt = match top with
  | TopLet b -> fprintf fmt "@[<v>let %t@]" (pp_binding names b)
  | TopRec bs -> fprintf fmt "@[<v>let rec %t" (pp_bindings names bs)

let pp_file names file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top names top fmt in
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
  | Unit -> Type.unit
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
