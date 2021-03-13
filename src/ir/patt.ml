open Format

(* Patterns *)

type atom =
  | Unit
  | Bool of { b: bool }
  | Int of { i: int }
  | Float of { f: float }
  | Rune of { r: bytes }
  | String of { length: int; s: string }
  | Byte of { b: bytes }
  | Blob of { length: int; bs: string }
  | Timestamp of { ts: string }
  | Duration of { d: string }
  | Ground of { ty: Type.t }
  | Var of { id: Sym.sym; ty: Type.t }
type compound =
  | Constr of { name: Sym.sym; arg: atom }
  | Atom of { atom: atom }

(* Constructors *)

let unit = Unit
let bool b = Bool { b }
let int i = Int { i }
let float f = Float { f }
let rune r = Rune { r }
let string s = String { length = String.length s; s }
let byte b = Byte { b }
let blob bs = Blob { length = Bytes.length bs; bs }
let timestamp ts = Timestamp { ts }
let duration d = Duration { d }
let ground = Ground
let var id ty = Var { id; ty }

let constr name arg = Constr { name; arg }
let atom atom = Atom { atom }
