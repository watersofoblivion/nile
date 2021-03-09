open Format

type un =
  | Not

let un_not = Not

let un_precedence = function
  | Not -> 2

type bin =
  | Add of Type.t
  | Sub of Type.t
  | Mul of Type.t
  | Div of Type.t
  | Mod
  | And
  | Or
  | Eq of Type.t
  | Neq of Type.t
  | Lte of Type.t
  | Lt of Type.t
  | Gt of Type.t
  | Gte of Type.t
  | Cons of Type.t

let bin_add ty = Add ty
let bin_sub ty = Sub ty
let bin_mul ty = Mul ty
let bin_div ty = Div ty
let bin_mod = Mod
let bin_and = And
let bin_or = Or
let bin_eq ty = Eq ty
let bin_neq ty = Neq ty
let bin_lte ty = Lte ty
let bin_lt ty = Lt ty
let bin_gt ty = Gt ty
let bin_gte ty = Gte ty
let bin_cons ty = Cons ty

let bin_precedence = function
  | Cons _ -> 2
  | Mul _ | Div _ | Mod -> 3
  | Add _ | Sub _ -> 4
  | Lte _ | Lt _ | Gt _ | Gte _ -> 6
  | Eq _ | Neq _ -> 7
  | And -> 11
  | Or -> 12
