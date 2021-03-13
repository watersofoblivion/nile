(* Builtins *)

type t =
  | LAnd
  | LOr
  | LNot
  | BAnd of { ty: Type.t }
  | BOr of { ty: Type.t }
  | BXor of { ty: Type.t }
  | BNot of { ty: Type.t }
  | Lsl of { ty: Type.t }
  | Lsr of { ty: Type.t }
  | Asl of { ty: Type.t }
  | Asr of { ty: Type.t }
  | Eq of { ty: Type.t }
  | Neq of { ty: Type.t }
  | Add of { ty: Type.t }
  | Sub of { ty: Type.t }
  | Mul of { ty: Type.t }
  | Div of { ty: Type.t }
  | Mod of { ty: Type.t }
  | Lte of { ty: Type.t }
  | Lt of { ty: Type.t }
  | Gt of { ty: Type.t }
  | Gte of { ty: Type.t }
  | Slice of { ty: Type.t }
  | Index of { ty: Type.t }
  | Concat of { ty: Type.t }
  | Before
  | After

let log_and = LAnd
let log_or = LOr
let log_not = LNot

let bit_and ty = BAnd { ty }
let bit_or ty = BOr { ty }
let bit_xor ty = BXor { ty }
let bit_not ty = BNot { ty }

let log_shl ty = Lsl { ty }
let log_shr ty = Lsr { ty }
let arith_shl ty = Asl { ty }
let arith_shr ty = Asr { ty }

let eq ty = Eq { ty }
let neq ty = Neq { ty }

let add ty = Add { ty }
let sub ty = Sub { ty }
let mul ty = Mul { ty }
let div ty = Div { ty }
let modulus ty = Mod { ty }

let lte ty = Lte { ty }
let lt ty = Lt { ty }
let gt ty = Gt { ty }
let gte ty = Gte { ty }

let slice ty = Slice { ty }
let index ty = Index { ty }
let concat ty = Concat { ty }

let before = Before
let after = After
