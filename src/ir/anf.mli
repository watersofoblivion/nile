open Format

(** {1 Administrative Normal Form Intermediate Representation} *)

(** {2 Unary Operators} *)

type un = private
  | Not (** Boolean negation *)
(** Unary operators *)

(** {3 Constructors} *)

val un_not : un
(** [un_not] constructs a boolean negation unary operator. *)

(** {3 Operations} *)

val pp_un : un -> formatter -> unit
(** [pp_un un] pretty-prints the unary operator [un] to the formatter [fmt]. *)

(** {2 Binary Operators} *)

type bin = private
  | Add (** Addition *)
  | Sub (** Subtraction *)
  | Mul (** Nultiplication *)
  | Div (** Integer Division *)
  | Mod (** Modululs *)
  | And (** Logical AND *)
  | Or  (** Logical OR *)
  | Eq  (** Equality *)
  | Neq (** Inequality *)
  | Lte (** Less Than or Equal *)
  | Lt  (** Less Than *)
  | Gt  (** Greater Than *)
  | Gte (** Greater Than or Eqaul *)
(** Binaroy operators *)

(** {3 Constructors} *)

val bin_add : bin
(** [bin_add] constructs an addition binary operator. *)

val bin_sub : bin
(** [bin_sub] constructs a subtraction binary operator. *)

val bin_mul : bin
(** [bin_mul] constructs a multiplication binary operator. *)

val bin_div : bin
(** [bin_mul] constructs an integer division binary operator. *)

val bin_mod : bin
(** [bin_mod] constructs a modulus binary operator. *)

val bin_and : bin
(** [bin_and] constructs a logical AND binary operator. *)

val bin_or : bin
(** [bin_or] constructs a logical OR binary operator. *)

val bin_eq : bin
(** [bin_eq] constructs an equality binary operator. *)

val bin_neq : bin
(** [bin_neq] constructs an inequality binary operator. *)

val bin_lte : bin
(** [bin_lte] constructs an less than or equal binary operator. *)

val bin_lt : bin
(** [bin_lt] constructs an less than binary operator. *)

val bin_gt : bin
(** [bin_gt] constructs an greater than binary operator. *)

val bin_gte : bin
(** [bin_gte] constructs an greater than or equal binary operator. *)

(** {3 Operations} *)

val pp_bin : bin -> formatter -> unit
(** [pp_bin bin] pretty-prints the binary operation [bin] to the formatter
    [fmt]. *)

(** {2 Primitives} *)

type prim = private
  | Bool of bool  (** Boolean *)
  | Int of int    (** Integer *)
  | Var of string (** Variable *)
(** Primitive values *)

(** {3 Constructors} *)

val prim_bool : bool -> prim
(** [prim_bool b] constructs a boolean primitive with the value [b]. *)

val prim_int : int -> prim
(** [prim_int i] constructs an integer primitive with the value [i]. *)

val prim_var : string -> prim
(** [prim_var id] constructs a variable primitive referencing the variable [id].
    *)

(** {3 Operations} *)

val pp_prim : prim -> formatter -> unit
(** [pp_prim prim fmt] pretty-prints the primitive value [prim] to the formatter
    [fmt]. *)

(** {2 Statements} *)

type stmt = private
  | UnOp of un * prim          (** Unary operation *)
  | BinOp of prim * bin * prim (** Binary operation *)
  | App of string * prim list  (** Function application *)
  | Prim of prim               (** A primitive *)
(** Statements *)

val stmt_un_op : un -> prim -> stmt
(** [stmt_un_op op r] constructs a unary operation statment applying the unary
    operator [op] to the primitive [r]. *)

val stmt_bin_op : prim -> bin -> prim -> stmt
(** [stmt_bin_op l op r] constructs a binary operation statment applying the
    binary operator [op] to the primitives [l] and [r]. *)

val stmt_app : string -> prim list -> stmt
(** [stmt_bin_op fn args] constructs a function application statment applying
    the function named [fn] to the primitive arguments [args]. *)

val stmt_prim : prim -> stmt
(** [stmt_prim p] constructs a primitive statment evaluating to the primitive
    value [p]. *)

(** {3 Operations} *)

val pp_stmt : stmt -> formatter -> unit
(** [pp_stmt stmt fmt] pretty-prints the statement [stmt] to the formatter
    [fmt]. *)

(* type t = private
  | Bind of string * Type.t * stmt * t
  | Rec of (string * Type.t * stmt) list * t
  | Abs of string * Type.t * t
  | If of prim * stmt * stmt

val bind : string -> Type.t -> stmt -> t -> t
val bind_rec : (string * Type.t * stmt) list -> t -> t
val abs : string -> Type.t -> t -> t
val cond : prim -> stmt -> stmt -> t

val pp : t -> formatter -> unit *)
