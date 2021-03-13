open Format

type un =
  | LNot
  | BNot of { ty: Type.t }

let un_lnot = LNot
let un_bnot ty = BNot { ty }

type bin =
  | BAnd of { ty: Type.t }
  | BOr of { ty: Type.t }
  | BXor of { ty: Type.t }
  | Lsl of { ty: Type.t }
  | Lsr of { ty: Type.t }
  | Asl of { ty: Type.t }
  | Asr of { ty: Type.t }
  | Add of { ty: Type.t }
  | Sub of { ty: Type.t }
  | Mul of { ty: Type.t }
  | Div of { ty: Type.t }
  | Mod
  | LAnd
  | LOr
  | Eq of { ty: Type.t }
  | Neq of { ty: Type.t }
  | Lte of { ty: Type.t }
  | Lt of { ty: Type.t }
  | Gt of { ty: Type.t }
  | Gte of { ty: Type.t }
  | Concat of { ty: Type.t }
  | Cons of { ty: Type.t }

let bin_band ty = BAnd { ty }
let bin_bor ty = BOr { ty }
let bin_bxor ty = BXor { ty }
let bin_lsl ty = Lsl { ty }
let bin_lsr ty = Lsr { ty }
let bin_asl ty = Asl { ty }
let bin_asr ty = Asr { ty }
let bin_add ty = Add { ty }
let bin_sub ty = Sub { ty }
let bin_mul ty = Mul { ty }
let bin_div ty = Div { ty }
let bin_mod = Mod
let bin_and = And
let bin_or = Or
let bin_eq ty = Eq { ty }
let bin_neq ty = Neq { ty }
let bin_lte ty = Lte { ty }
let bin_lt ty = Lt { ty }
let bin_gt ty = Gt { ty }
let bin_gte ty = Gte { ty }
let bin_concat ty = Concat { ty }
let bin_cons ty = Cons { ty }
