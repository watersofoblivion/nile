open Format

(** {1 Patterns} *)

type t =
  | Ground         (** Ground *)
  | Unit           (** Unit *)
  | Bool of bool   (** Boolean *)
  | Int of int     (** Integer *)
  | Var of Sym.sym (** Variable *)
(** A pattern *)

(** {2 Constructors} *)

val ground : t
(** [ground] constructs a ground pattern *)

val unit : t
(** [unit] constructs a unit pattern. *)

val bool : bool -> t
(** [bool b] constructs a boolean pattern matching [b]. *)

val int : int -> t
(** [int i] constructs an integer pattern matching [i]. *)

val var : Sym.sym -> t
(** [var sym] constructs a variable pattern binding the matched value to [sym].
    *)

(** {2 Pretty Printing} *)

val pp : Sym.names -> t -> formatter -> unit
(** [pp names patt fmt] pretty-prints the pattern [patt] to the formatter [fmt]
    using [names] to resolve symbol names. *)

(** {2 Type Checking} *)

val irrefutable : t -> bool
(** [irrefutable patt] tests whether or not the pattern [patt] is irrefutable.
    That is, whether it will match all values of a type [ty] for which
    [matches_type patt ty] returns [true]. *)
