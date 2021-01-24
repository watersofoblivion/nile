open Format

(** {1 Patterns} *)

type t =
  | Ground         (** Ground *)
  | Bool of bool   (** Boolean *)
  | Int of int     (** Integer *)
  | Var of Sym.sym (** Variable *)
(** A pattern *)

(** {2 Constructors} *)

val ground : t
(** [ground] constructs a ground pattern *)

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
