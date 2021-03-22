open Common

type un =
  | LNot of { loc: Loc.t }
  | BNot of { loc: Loc.t }

let un_lnot loc = LNot { loc }
let un_bnot loc = BNot { loc }

type bin =
  | BAnd of { loc: Loc.t }
  | BOr of { loc: Loc.t }
  | BXor of { loc: Loc.t }
  | Lsl of { loc: Loc.t }
  | Lsr of { loc: Loc.t }
  | Asl of { loc: Loc.t }
  | Asr of { loc: Loc.t }
  | Add of { loc: Loc.t }
  | Sub of { loc: Loc.t }
  | Mul of { loc: Loc.t }
  | Div of { loc: Loc.t }
  | Mod of { loc: Loc.t }
  | LAnd of { loc: Loc.t }
  | LOr of { loc: Loc.t }
  | Eq of { loc: Loc.t }
  | Neq of { loc: Loc.t }
  | Lte of { loc: Loc.t }
  | Lt of { loc: Loc.t }
  | Gt of { loc: Loc.t }
  | Gte of { loc: Loc.t }
  | Concat of { loc: Loc.t }
  | Cons of { loc: Loc.t }

let bin_band loc = BAnd { loc }
let bin_bor loc = BOr { loc }
let bin_bxor loc = BXor { loc }
let bin_lsl loc = Lsl { loc }
let bin_lsr loc = Lsr { loc }
let bin_asl loc = Asl { loc }
let bin_asr loc = Asr { loc }
let bin_add loc = Add { loc }
let bin_sub loc = Sub { loc }
let bin_mul loc = Mul { loc }
let bin_div loc = Div { loc }
let bin_mod loc = Mod { loc }
let bin_and loc = And { loc }
let bin_or loc = Or { loc }
let bin_eq loc = Eq { loc }
let bin_neq loc = Neq { loc }
let bin_lte loc = Lte { loc }
let bin_lt loc = Lt { loc }
let bin_gt loc = Gt { loc }
let bin_gte loc = Gte { loc }
let bin_concat loc = Concat { loc }
let bin_cons loc = Cons { loc }
