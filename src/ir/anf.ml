open Format

type un =
  | Not

let un_not = Not

let pp_un un fmt = match un with
  | Not -> fprintf fmt "!"

type bin =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Lte
  | Lt
  | Gt
  | Gte

let bin_add = Add
let bin_sub = Sub
let bin_mul = Mul
let bin_div = Div
let bin_mod = Mod
let bin_and = And
let bin_or = Or
let bin_eq = Eq
let bin_neq = Neq
let bin_lte = Lte
let bin_lt = Lt
let bin_gt = Gt
let bin_gte = Gte

let pp_bin bin fmt = match bin with
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Mod -> fprintf fmt "%%"
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "||"
  | Eq -> fprintf fmt "=="
  | Neq -> fprintf fmt "!="
  | Lte -> fprintf fmt "<="
  | Lt -> fprintf fmt "<"
  | Gt -> fprintf fmt ">"
  | Gte -> fprintf fmt ">="

type prim =
  | Bool of bool
  | Int of int
  | Var of string

let prim_bool b = Bool b
let prim_int i = Int i
let prim_var id = Var id

let pp_prim p fmt = match p with
  | Bool b -> fprintf fmt "%b" b
  | Int i -> fprintf fmt "%d" i
  | Var v -> fprintf fmt "%s" v

type stmt =
  | UnOp of un * prim
  | BinOp of prim * bin * prim
  | App of string * prim list
  | Prim of prim

let stmt_un_op op r = UnOp (op, r)
let stmt_bin_op l op r = BinOp (l, op, r)
let stmt_app fn args = App (fn, args)
let stmt_prim p = Prim p

let pp_stmt stmt fmt = match stmt with
  | UnOp (op, r) -> fprintf fmt "%t%t" (pp_un op) (pp_prim r)
  | BinOp (l, op, r) -> fprintf fmt "%t %t %t" (pp_prim l) (pp_bin op) (pp_prim r)
  | App (fn, args) ->
    fprintf fmt "%s " fn;
    let pp_sep fmt _ = fprintf fmt " " in
    let pp_arg fmt arg = pp_prim arg fmt in
    pp_print_list ~pp_sep pp_arg fmt args
  | Prim p -> pp_prim p fmt

(* type t =
  | Bind of string * Type.t * stmt * t
  | Rec of (string * Type.t * stmt) list * t
  | Abs of string * Type.t * t
  | If of prim * stmt * stmt

let bind id ty stmt rest = Bind (id, ty, stmt, rest)
let bind_rec bindings rest = Rec (bindings, rest)
let abs id ty expr = Abs (id, ty, expr)
let cond c t f = If (c, t, f)

let rec pp_bindings bindings fmt = match bindings with
  | [] -> ()
  | (id, ty, stmt) :: [] -> fprintf fmt "%s: %t = %t" id (Type.pp ty) (pp_stmt stmt)
  | (id, ty, stmt) :: bindings ->
    fprintf fmt "%s: %t = %t and " id (Type.pp ty) (pp_stmt stmt);
    pp_bindings bindings fmt

let rec pp expr fmt = match expr with
  | Bind (id, ty, stmt, rest) -> fprintf fmt "let %s: %t = %t in %t" id (Type.pp ty) (pp_stmt stmt) (pp rest)
  | Rec (bindings, rest) -> fprintf fmt "let rec %t in %t" (pp_bindings bindings) (pp rest)
  | Abs (id, ty, expr) -> fprintf fmt "%s: %t. %t" id (Type.pp ty) (pp expr)
  | If (c, t, f) -> fprintf fmt "if %t then %t else %t" (pp_prim c) (pp_stmt t) (pp_stmt f) *)
