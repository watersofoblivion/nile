open Format

(** {1 Patterns} *)

type t =
  | Ground                       (** Ground *)
  | Nil                          (** Empty list *)
  | Unit                         (** Unit *)
  | Bool of bool                 (** Boolean *)
  | Int of int                   (** Integer *)
  | Float of float               (** Floating-point *)
  | String of string             (** String *)
  | Var of Sym.sym               (** Variable *)
  | Tuple of int * t list        (** Tuple *)
  | Record of field list * bool  (** Record *)
  | Cons of t * t                (** Cons *)
  | Or of t list                 (** Or *)
(** A pattern *)

and field =
  | Bare of Sym.sym      (** Bare field name *)
  | Named of Sym.sym * t (** Named field *)
(** Record field *)

(** {2 Constructors} *)

val ground : t
(** [ground] constructs a ground pattern *)

val nil : t
(** [nil] constructs a nil pattern *)

val unit : t
(** [unit] constructs a unit pattern. *)

val bool : bool -> t
(** [bool b] constructs a boolean pattern matching [b]. *)

val int : int -> t
(** [int i] constructs an integer pattern matching [i]. *)

val float : float -> t
(** [float f] constructs an floating-point pattern matching [f]. *)

val string : string -> t
(** [string s] constructs an string pattern matching [s]. *)

val var : Sym.sym -> t
(** [var sym] constructs a variable pattern binding the matched value to [sym].
    *)

val tuple : t list -> t
(** [tuple patts] constructs a tuple pattern matching a tuple when the element
    patterns [patts] match their corresponding elements. *)

val record : field list -> bool -> t
(** [record fields elipsis] constructs a record pattern matching a record when
    the field patterns [fields] match their corresponding fields.  If [elipsis]
    is [true], then all fields of the record not provided are matched against
    the ground ("[_]") pattern. *)

val or : t list -> t
(** [or patts] constructs an or pattern matching any of the patters [patts]. *)

val bare : Sym.sym -> field
(** [bare name] constructs a pattern that matches a record field. The value of
    the field is matched to a variable named identically to the field. *)

val named : Sym.sym -> t -> field
(** [named name patt] constructs a pattern that matches a record field. The
    value of the field must match [patt]. *)

(** {2 Pretty Printing} *)

val pp : Sym.names -> t -> formatter -> unit
(** [pp names patt fmt] pretty-prints the pattern [patt] to the formatter [fmt]
    using [names] to resolve symbol names. *)

(** {2 Type Checking} *)

val irrefutable : t -> bool
(** [irrefutable patt] tests whether or not the pattern [patt] is irrefutable.
    That is, whether it will match all values of a type [ty] for which
    [matches_type patt ty] returns [true]. *)
