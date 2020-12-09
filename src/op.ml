open Format

type un =
  | Not
  | Neg

let un_not = Not
let un_neg = Neg

let pp_un op fmt = match op with
  | Not -> fprintf fmt "!"
  | Neg -> fprintf fmt "-"

let un_equal op op' = match op, op' with
  | Not, Not | Neg, Neg -> true
  | _ -> false

let un_precedence = function
  | Not | Neg -> 2

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

let pp_bin op fmt = match op with
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

let bin_equal op op' = match op, op' with
  | Add, Add | Sub, Sub | Mul, Mul | Div, Div | Mod, Mod
  | And, And | Or, Or
  | Eq, Eq | Neq, Neq | Lte, Lte | Lt, Lt | Gt, Gt | Gte, Gte -> true
  | _ -> false

let bin_precedence = function
  | Mul | Div | Mod -> 3
  | Add | Sub -> 4
  | Lte | Lt | Gt | Gte -> 6
  | Eq | Neq -> 7
  | And -> 11
  | Or -> 12
