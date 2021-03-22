open Format

(** {1 Types} *)

type t = private
  | Unit (** Unit *)
  | Bool (** Boolean *)
  | Int (** Integer *)
  | Float (** Floating-point *)
  | Rune (** Rune *)
  | String (** String *)
  | Byte (** Byte *)
  | Blob (** Binary Large Object (BLOB) *)
  | Timestamp (** ISO-8601 Timestamp *)
  | Duration (** ISO-8601 Duration *)
  | Fun of {
      params: t list; (** Parameter Types *)
      res:    t       (** Result Type *)
    } (** Function *)
  | Tuple of {
      arity: int;   (** Arity *)
      types: t list (** Element Types *)
    } (** Tuple *)
  | Variant of {
      constrs: constr list (** Constructors *)
    } (** Variant *)
(** Types *)

and constr = Constr of {
  name:   Sym.sym; (** Name *)
  params: t list   (** Parameters *)
}
(** Variant constructor *)

(** {2 Constructors} *)

val unit : t
(** [unit] constructs a unit type. *)

val bool : t
(** [bool] constructs a boolean type. *)

val int : t
(** [int] constructs an integer type. *)

val float : t
(** [float] constructs a floating-point type. *)

val string : t
(** [string] constructs a string type. *)

val byte : t
(** [byte] constructs a byte type. *)

val blob : t
(** [blob] constructs a binary large object (BLOB) type. *)

val timestamp : t
(** [timestamp] constructs a ISO-8601 timestamp type. *)

val duration : t
(** [duration] constructs a ISO-8601 duration type. *)

val func : t list -> t -> t
(** [func args ret] constructs a function type mapping argument values of types
    [args] to values of type [ret]. *)

val tuple : t list -> t
(** [tuple tys] constructs a tuple type with element types [tys]. *)

val variant : constr list -> t
(** [variant constrs] constructs a variant type with constructors [constrs]. *)

val constr : Sym.sym -> t option -> field
(** [constr id ty] constructs a variant constructor named [id] with optional
    type [ty]. *)

(** {2 Operations} *)

val equal : t -> t -> bool
(** [equal x y] compares two types for equality. *)
