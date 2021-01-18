open Format

(** {1 Types} *)

type t = private
  | Unit                  (** Unit *)
  | Bool                  (** Booleans *)
  | Int                   (** Integers *)
  | Fun of t * t          (** Functions *)
  | Tuple of int * t list (** Tuple *)
(** Types *)

(** {2 Constructors} *)

val unit : t
(** [unit] constructs a unit type *)

val bool : t
(** [bool] constructs a boolean type *)

val int : t
(** [int] constructs an integer type *)

val func : t -> t -> t
(** [func a b] constructs a function type mapping values of type [a] to values
    of type [b]. *)

val tuple : t list -> t
(** [tuple tys] constructs a tuple type with element types [tys]. *)

(** {2 Operations} *)

val pp : t -> formatter -> unit
(** [pp ty fmt] pretty-prints [ty] to the formatter [fmt]. *)

val equal : t -> t -> bool
(** [equal x y] compares two types for equality. *)
