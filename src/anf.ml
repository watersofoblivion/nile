open Format

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
  | UnOp of Op.un * prim
  | BinOp of prim * Op.bin * prim
  | App of prim * prim
  | Tail of prim * prim

let stmt_un_op op r = UnOp (op, r)
let stmt_bin_op l op r = BinOp (l, op, r)
let stmt_app f x = App (f, x)
let stmt_tail f x = Tail (f, x)

let pp_stmt stmt fmt = match stmt with
  | UnOp (op, r) -> fprintf fmt "%t %t" (Op.pp_un op) (pp_prim r)
  | BinOp (l, op, r) -> fprintf fmt "%t %t %t" (pp_prim l) (Op.pp_bin op) (pp_prim r)
  | App (f, x) -> fprintf fmt "%t %t" (pp_prim f) (pp_prim x)
  | Tail (f, x) -> fprintf fmt "%t %t" (pp_prim f) (pp_prim x)

type t =
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
  | If (c, t, f) -> fprintf fmt "if %t then %t else %t" (pp_prim c) (pp_stmt t) (pp_stmt f)
