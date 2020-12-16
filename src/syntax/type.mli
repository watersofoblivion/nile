open Format

(** {1 Types} *)

type t = private
  | Int of Loc.t (** Integers *)
  | Bool of Loc.t (** Booleans *)
  | Fun of Loc.t * t * t (** Functions *)
(** Types *)

(** {2 Constructors} *)

val int : Loc.t -> t
(** [int loc] constructs an integer type *)

val bool : Loc.t -> t
(** [bool loc] constructs a boolean type *)

val func : Loc.t -> t -> t -> t
(** [func loc a b] constructs a function type from [a] to [b]. *)

(** {2 Operations} *)

val pp : t -> formatter -> unit
(** [pp ty fmt] pretty-prints [ty] to the formatter [fmt]. *)

val equal : t -> t -> bool
(** [equal x y] compares two types for equality. *)

val loc : t -> Loc.t
(** [loc ty] returns the location information associated with [ty]. *)
