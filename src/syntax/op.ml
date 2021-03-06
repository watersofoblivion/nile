open Format

type un =
  | Not

let un_not = Not

let pp_un op fmt = match op with
  | Not -> fprintf fmt "!"

let un_precedence = function
  | Not -> 2

exception InvalidUnaryOperand of Type.t * un * Type.t

let invalid_unary_operand expected op actual =
  InvalidUnaryOperand (expected, op, actual)
    |> raise

let type_of_un op r = match op, r with
  | Not, Type.Bool -> Type.bool
  | Not, r -> invalid_unary_operand Type.bool op r

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
  | Dot
  | Cons

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
let bin_dot = Dot
let bin_cons = Cons

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
  | Dot -> fprintf fmt "."
  | Cons -> fprintf fmt "::"

let bin_precedence = function
  | Dot | Cons -> 2
  | Mul | Div | Mod -> 3
  | Add | Sub -> 4
  | Lte | Lt | Gt | Gte -> 6
  | Eq | Neq -> 7
  | And -> 11
  | Or -> 12

exception InvalidBinaryOperands of Type.t * Type.t * bin * Type.t
exception InvalidEqualityOperands of Type.t * bin * Type.t

let invalid_binary_operands expected actual op actual' =
  InvalidBinaryOperands (expected, actual, op, actual')
    |> raise

let invalid_equality_operands actual op actual' =
  InvalidEqualityOperands (actual, op, actual')
    |> raise

let type_of_arith l op r = match l, r with
  | Type.Int, Type.Int -> Type.int
  | l, r -> invalid_binary_operands Type.int l op r

let type_of_bool l op r = match l, r with
  | Type.Bool, Type.Bool -> Type.bool
  | l, r -> invalid_binary_operands Type.bool l op r

let type_of_eq l op r = match l, r with
  | Type.Int, Type.Int
  | Type.Bool, Type.Bool -> Type.bool
  | l, r -> invalid_equality_operands l op r

let type_of_cmp l op r = match l, r with
  | Type.Int, Type.Int -> Type.bool
  | l, r -> invalid_binary_operands Type.int l op r

let type_of_bin l op r =
  match op with
    | Add | Sub | Mul | Div | Mod -> type_of_arith l op r
    | And | Or -> type_of_bool l op r
    | Eq | Neq -> type_of_eq l op r
    | Lte | Lt | Gt | Gte -> type_of_cmp l op r
