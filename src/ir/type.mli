open Format

(** {1 Types} *)

type t = private
  | Bool         (** Boolean *)
  | Int          (** Integer *)
  | Fun of t * t (** Function *)
(** Types *)

(** {2 Constructors} *)

val bool : t
(** [bool] constructs a boolean type. *)

val int : t
(** [int] constructs a integer type. *)

val func : t -> t -> t
(** [func a b] constructs a function type with an argument of type [a] and a
    result of type [b]. *)

(** {2 Operations} *)

val pp : t -> formatter -> unit
(** [pp ty fmt] pretty-prints the type [ty] to the formatter [fmt]. *)
