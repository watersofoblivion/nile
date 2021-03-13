open Format

(* Patterns *)

type t =
  | Unit
  | Bool of { b: bool }
  | Int of { i: int }
  | Float of { f: float }
  | Rune of { r: bytes }
  | String of { s: string }
  | Byte of { b: bytes }
  | Blob of { bs: bytes }
  | Timestamp of { ts: string }
  | Duration of { d: string }
  | Ground of { ty: Type.t }
  | Var of { id: Sym.sym; ty: Type.t }
  | Tuple of { arity: int; patts: t list; ty: Type.t }
  | Record of { fields: field list; ty: Type.t }
  | Or of { patts: t list }
and field = Field of { name: Sym.sym; patt: t }

(* Constructors *)

let unit = Unit
let bool b = Bool { b }
let int i = Int { i }
let float f = Float { f }
let rune r = Rune { r }
let string s = String { s }
let byte b = Byte { b }
let blob bs = Blob { bs }
let timestamp ts = Timestamp { ts }
let duration d = Duration { d }
let ground ty = Ground { ty }
let var id ty = Var { id; ty }
let tuple patts ty = Tuple { arity = List.length patts; patts; ty }
let record fields ty = Record { fields; ty }
let orr patts = Or { patts }

let field name patt = Field { name; patt }
