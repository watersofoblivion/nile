open Format

(** {1 Patterns} *)

type t =
  | Ground of Loc.t                     (** Ground *)
  | Nil of Loc.t                        (** Empty list *)
  | Unit of Loc.t                       (** Unit *)
  | Bool of Loc.t * bool                (** Boolean *)
  | Int of Loc.t * int                  (** Integer *)
  | Float of Loc.t * float              (** Floating-point *)
  | String of Loc.t * string            (** String *)
  | Var of Loc.t * Sym.sym              (** Variable *)
  | Tuple of Loc.t * int * t list       (** Tuple *)
  | Record of Loc.t * field list * bool (** Record *)
  | Cons of Loc.t * t * t               (** Cons *)
  | Or of Loc.t * t list                (** Or *)
(** A pattern *)

and field =
  | Bare of Loc.t * Sym.sym      (** Bare field name *)
  | Named of Loc.t * Sym.sym * t (** Named field *)
(** Record field *)

(** {2 Constructors} *)

val ground : Loc.t -> t
(** [ground loc] constructs a ground pattern at location [loc]. *)

val nil : Loc.t -> t
(** [nil loc] constructs a nil pattern at location [loc]. *)

val unit : Loc.t -> t
(** [unit loc] constructs a unit pattern at location [loc]. *)

val bool : Loc.t -> bool -> t
(** [bool loc b] constructs a boolean pattern matching [b] at location [loc]. *)

val int : Loc.t -> int -> t
(** [int loc i] constructs an integer pattern matching [i] at location [loc]. *)

val float : Loc.t -> float -> t
(** [float loc f] constructs an floating-point pattern matching [f] at location
    [loc]. *)

val string : Loc.t -> string -> t
(** [string loc s] constructs an string pattern matching [s] at location [loc].
    *)

val var : Loc.t -> Sym.sym -> t
(** [var loc sym] constructs a variable pattern at location [loc] binding the
    matched value to [sym]. *)

val tuple : Loc.t -> t list -> t
(** [tuple loc patts] constructs a tuple pattern at location [loc] matching a
    tuple when the element patterns [patts] match their corresponding elements.
    *)

val record : Loc.t -> field list -> bool -> t
(** [record loc fields elipsis] constructs a record pattern at location [loc]
    matching a record when the field patterns [fields] match their corresponding
    fields.  If [elipsis] is [true], then all fields of the record not provided
    are matched against the ground ("[_]") pattern. *)

val orr : Loc.t -> t list -> t
(** [orr loc patts] constructs an "or" pattern at location [loc] matching any of
    the patters [patts]. *)

val bare : Loc.t -> Sym.sym -> field
(** [bare loc name] constructs a pattern at location [loc] that matches a record
    field. The value of the field is matched to a variable named identically to
    the field. *)

val named : Loc.t -> Sym.sym -> t -> field
(** [named loc name patt] constructs a pattern at location [loc] that matches a
    record field. The value of the field must match [patt]. *)

(** {2 Operations} *)

val loc : t -> Loc.t
(** [loc patt] returns the location of the pattern [patt]. *)

(** {2 Type Checking} *)

val irrefutable : t -> bool
(** [irrefutable patt] tests whether or not the pattern [patt] is irrefutable.
    That is, whether it will match all values of a type [ty] for which
    [matches_type patt ty] returns [true]. *)
