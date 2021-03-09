open Format

(** {1 Patterns} *)

type t =
  | Ground                (** Ground *)
  | Nil                   (** Empty list *)
  | Unit                  (** Unit *)
  | Bool of bool          (** Boolean *)
  | Int of int            (** Integer *)
  | Float of float        (** Floating-point *)
  | String of string      (** String *)
  | Var of Sym.sym        (** Variable *)
  | Tuple of int * t list (** Tuple *)
  | Record of field list  (** Record *)
  | Cons of t * t         (** Cons *)
  | Or of t list          (** Or *)
(** A pattern *)

and field = Sym.sym * t
(** Record field *)

(** {2 Constructors} *)

val ground : t
(** [ground] constructs a ground pattern. *)

val nil : t
(** [nil] constructs a nil pattern. *)

val unit : t
(** [unit] constructs a unit pattern. *)

val bool : bool -> t
(** [bool b] constructs a boolean pattern matching [b]. *)

val int : int -> t
(** [int i] constructs an integer pattern matching [i]. *)

val float : float -> t
(** [float f] constructs an floating-point pattern matching [f].*)

val string : string -> t
(** [string s] constructs an string pattern matching [s]. *)

val var : Sym.sym -> t
(** [var sym] constructs a variable pattern binding the matched value to [sym].
    *)

val tuple : t list -> t
(** [tuple patts] constructs a tuple pattern matching a tuple when the element
    patterns [patts] match their corresponding elements. *)

val record : field list -> t
(** [record fields] constructs a record pattern matching a record when the field
    patterns [fields] match their corresponding fields. *)

val orr : t list -> t
(** [orr patts] constructs an "or" pattern matching any of the patters [patts].
    *)

val field : Sym.sym -> t -> field
(** [field name patt] constructs a pattern that matches a record field. The
    value of the field must match [patt]. *)
