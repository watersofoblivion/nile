open Format
open Common

(* Syntax *)

type atom =
  | Bool of bool
  | Int of int
  | Var of int
  | Abs of int * Type.t * Type.t * block
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
and binding = int * Type.t * expr

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let bool b = Bool b
let int i = Int i
let var idx = Var idx
let abs idx arg res body = Abs (idx, arg, res, body)

let un_op op r = UnOp (op, r)
let bin_op l op r = BinOp (l, op, r)
let app f x = App (f, x)
let atom a = Atom a

let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let cond c t f = If (c, t, f)
let expr c = Expr c

let binding idx ty expr = (idx, ty, expr)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops

(* Pretty-Printing *)

let rec pp_atom atom fmt = match atom with
  | Bool b -> fprintf fmt "%b" b
  | Int i -> fprintf fmt "%d" i
  | Var idx -> fprintf fmt "$%d" idx
  | Abs (id, arg, res, body) -> pp_abs id arg res body fmt

and pp_abs id arg res body fmt =
  fprintf fmt "@[<hv>($%d: %t): %t =>@;<1 2>%t@]" id (Type.pp arg) (Type.pp res) (pp_block body)

and pp_expr expr fmt = match expr with
  | UnOp (op, r) -> pp_un_op op r fmt
  | BinOp (l, op, r) -> pp_bin_op l op r fmt
  | App (f, x) -> pp_app f x fmt
  | Atom atom -> pp_atom atom fmt

and pp_un_op op r fmt =
  fprintf fmt "%t%t" (Op.pp_un op) (pp_atom r)

and pp_bin_op l op r fmt =
  fprintf fmt "%t %t %t" (pp_atom l) (Op.pp_bin op) (pp_atom r)

and pp_app f x fmt =
  fprintf fmt "%t %t" (pp_atom f) (pp_atom x)

and pp_block block fmt = match block with
  | Let (b, rest) -> pp_bind b rest fmt
  | LetRec (bs, rest) -> pp_bind_rec bs rest fmt
  | If (c, t, f) ->  pp_cond c t f fmt
  | Expr expr -> pp_expr expr fmt

and pp_bind b rest fmt =
  fprintf fmt "@[<v>let %t in@ %t@]" (pp_binding b) (pp_block rest)

and pp_bind_rec bs rest fmt =
  fprintf fmt "@[<v>let rec %t in@ %t@]" (pp_bindings bs) (pp_block rest)

and pp_cond c t f fmt =
  fprintf fmt "@[<v>if %t@ then %t@ else %t@]" (pp_atom c) (pp_block t) (pp_block f)

and pp_bindings bs fmt =
  let pp_sep fmt _ = fprintf fmt " " in
  let pp_b b fmt = pp_binding fmt b in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding (id, ty, e) fmt =
  fprintf fmt "$%d: %t = %t" id (Type.pp ty) (pp_expr e)

let pp_top top fmt = match top with
  | TopLet b -> fprintf fmt "@[<v>let %t@]" (pp_binding b)
  | TopRec bs -> fprintf fmt "@[<v>let rec %t" (pp_bindings bs)

let pp_file file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top top fmt in
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
  | Var idx -> type_of_var env idx
  | Abs (idx, ty, res, body) -> type_of_abs env idx ty res body

and type_of_var env idx =
  try Check.lookup idx env
  with Not_found -> Check.unbound_identifier idx

and type_of_abs env idx ty res body =
  let env = Check.bind idx ty env in
  let res' = type_of_block env body in
  if Type.equal res res'
  then res
  else Check.declaration_mismatch idx res res'

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
  | Let ((idx, ty, expr), rest) -> type_of_bind env idx ty expr rest
  | LetRec (bs, rest) -> type_of_bind_rec env bs rest
  | If (c, t, f) -> type_of_cond env c t f
  | Expr expr -> type_of_expr env expr

and type_of_bind env idx ty expr rest =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then
    let env = Check.bind idx ty env in
    type_of_block env rest
  else Check.declaration_mismatch idx ty expr

and type_of_bind_rec env bs rest =
  let fold env (idx, ty, _) = Check.bind idx ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (idx, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Check.declaration_mismatch idx ty expr
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

let type_of_top_bind env idx ty expr =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then Check.bind idx ty env
  else Check.declaration_mismatch idx ty expr

let type_of_top_bind_rec env bs =
  let fold env (idx, ty, _) = Check.bind idx ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (idx, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Check.declaration_mismatch idx ty expr
    in
    List.iter iter bs
  in
  env

let type_of_top env = function
  | TopLet (idx, ty, expr) -> type_of_top_bind env idx ty expr
  | TopRec bs -> type_of_top_bind_rec env bs

let type_of_file = List.fold_left type_of_top
