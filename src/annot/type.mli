open Format

(** {1 Types} *)

type t = private
  | Unit (** Unit *)
  | Bool (** Booleans *)
  | Int (** Integers *)
  | Float (** Floating-point *)
  | Rune (** Rune *)
  | String (** Strings *)
  | Byte (** Byte *)
  | Blob (** Binary Large Object (BLOB) *)
  | Timestamp (** ISO-8601 Timestamp *)
  | Duration (** ISO-8601 Duration *)
  | Fun of {
      params: t list; (** Parameters *)
      res:    t       (** Result *)
    } (** Functions *)
  | Tuple of {
      arity: int;   (** Arity *)
      types: t list (** Element Types *)
    } (** Tuple *)
  | Record of {
      fields: field list (** Fields *)
    } (** Record *)
  | Variant of {
      constrs: constr list (** Constructors *)
    } (** Variant *)
  | Named of {
      name: Sym.sym (** Declared Type *)
    } (** Named Type *)
(** Types *)

and field = Field of {
  name: Sym.sym; (** Name *)
  ty:   t        (** Type *)
}
(** Record field *)

and constr = Constr of {
  name: Sym.sym; (** Name *)
  params: t list (** Parameters *)
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

val rune : t
(** [rune] constructs a rune type. *)

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
(** [func params ret] constructs a function type with parameters of types
    [params] and returning values of type [ret]. *)

val tuple : t list -> t
(** [tuple types] constructs a tuple type with element types [types]. *)

val record : field list -> t
(** [record fields] constructs a record type with fields [fields]. *)

val variant : constr list -> t
(** [variant constrs] constructs a variant type with constructors [constrs]. *)

val named : Sym.map -> t
(** [named name] constructs a named type referencing [name]. *)

val field : Sym.sym -> t -> field
(** [field name ty] constructs a record field named [name] of type [ty]. *)

val constr : Sym.sym -> t list -> field
(** [constr name params] constructs a variant constructor named [name] with
    parameters [params]. *)

(** {2 Operations} *)

val equal : t -> t -> bool
(** [equal x y] compares two types for equality. *)

(** {2 Type Checking} *)

(** {3 Environments} *)

type env
(** A type environment *)

val env : env
(** [env] constructs an empty type environment. *)

val bind : Patt.t -> t -> env -> env
(** [bind patt ty env] constructs copy of [env] extended by binding the type
    [ty] to the pattern [patt].  Any type previously bound to [patt] is masked.
    The original environment is left unaltered. *)

val lookup : Sym.sym -> env -> t
(** [lookup sym env] finds the type bound to the symbol [sym] in the environment
    [env], or raises {!Not_found} if the symbol is unbound. *)

(** {3 Patterns} *)

val of_pattern : Patt.t -> t -> bool
(** [of_pattern patt ty] tests whether or not the pattern [patt] matches
    values of type [ty]. *)
