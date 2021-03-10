type un =
  | Not of Loc.t

let un_not loc = Not loc

let un_loc = function
  | Not loc -> loc

let un_precedence = function
  | Not _ -> 2

exception InvalidUnaryOperand of Type.t * un * Type.t

let invalid_unary_operand expected op actual =
  InvalidUnaryOperand (expected, op, actual)
    |> raise

let type_of_un op r = match op, r with
  | Not _, Type.Bool -> Type.bool
  | Not _, r -> invalid_unary_operand Type.bool op r

type bin =
  | Add of Loc.t
  | Sub of Loc.t
  | Mul of Loc.t
  | Div of Loc.t
  | Mod of Loc.t
  | And of Loc.t
  | Or of Loc.t
  | Eq of Loc.t
  | Neq of Loc.t
  | Lte of Loc.t
  | Lt of Loc.t
  | Gt of Loc.t
  | Gte of Loc.t
  | Dot of Loc.t

let bin_add loc = Add loc
let bin_sub loc = Sub loc
let bin_mul loc = Mul loc
let bin_div loc = Div loc
let bin_mod loc = Mod loc
let bin_and loc = And loc
let bin_or loc = Or loc
let bin_eq loc = Eq loc
let bin_neq loc = Neq loc
let bin_lte loc = Lte loc
let bin_lt loc = Lt loc
let bin_gt loc = Gt loc
let bin_gte loc = Gte loc
let bin_dot loc = Dot loc

let bin_loc = function
  | Add loc
  | Sub loc
  | Mul loc
  | Div loc
  | Mod loc
  | And loc
  | Or loc
  | Eq loc
  | Neq loc
  | Lte loc
  | Lt loc
  | Gt loc
  | Gte loc
  | Dot loc -> loc

let bin_precedence = function
  | Dot _ -> 2
  | Mul _ | Div _ | Mod _ -> 3
  | Add _ | Sub _ -> 4
  | Lte _ | Lt _ | Gt _ | Gte _ -> 6
  | Eq _ | Neq _ -> 7
  | And _ -> 11
  | Or _ -> 12

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
    | _ -> failwith "Dot or Cons"
