open Format

(** {1 Types} *)

type t = private
  | Int (** Integers *)
  | Bool (** Booleans *)
  | Fun of t * t (** Functions *)
(** Types *)

(** {2 Constructors} *)

val int : t
(** [int] constructs an integer type *)

val bool : t
(** [bool] constructs a boolean type *)

val func : t -> t -> t
(** [func a b] constructs a function type from [a] to [b]. *)

(** {2 Operations} *)

val pp : t -> formatter -> unit
(** [pp ty fmt] pretty-prints [ty] to the formatter [fmt]. *)

val equal : t -> t -> bool
(** [equal x y] compares two types for equality. *)
