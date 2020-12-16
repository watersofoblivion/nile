(* open Format

type prim = private
  | Bool of bool
  | Int of int
  | Var of string

val prim_bool : bool -> prim
val prim_int : int -> prim
val prim_var : string -> prim

val pp_prim : prim -> formatter -> unit

type stmt = private
  | UnOp of Op.un * prim
  | BinOp of prim * Op.bin * prim
  | App of prim * prim
  | Tail of prim * prim

val stmt_un_op : Op.un -> prim -> stmt
val stmt_bin_op : prim -> Op.bin -> prim -> stmt
val stmt_app : prim -> prim -> stmt
val stmt_tail : prim -> prim -> stmt

val pp_stmt : stmt -> formatter -> unit

type t = private
  | Bind of string * Type.t * stmt * t
  | Rec of (string * Type.t * stmt) list * t
  | Abs of string * Type.t * t
  | If of prim * stmt * stmt

val bind : string -> Type.t -> stmt -> t -> t
val bind_rec : (string * Type.t * stmt) list -> t -> t
val abs : string -> Type.t -> t -> t
val cond : prim -> stmt -> stmt -> t

val pp : t -> formatter -> unit *)
