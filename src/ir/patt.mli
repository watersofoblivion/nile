open Format

(** {1 Patterns} *)

type atom =
  | Ground           (** Ground *)
  | Nil              (** Empty list *)
  | Unit             (** Unit *)
  | Bool of bool     (** Boolean *)
  | Int of int       (** Integer *)
  | Float of float   (** Floating-point *)
  | String of string (** String *)
  | Var of Sym.sym   (** Variable *)
(** Atomic Patterns *)

type compound =
  | Tuple of int * atom list        (** Tuple *)
  | Cons of atom * atom             (** Cons *)
  | Constr of Sym.sym * atom option (** Constructor *)
(** Compound patterns *)

(** {2 Constructors} *)

(** {3 Atomic Patterns} *)

val ground : atom
(** [ground] constructs a ground pattern. *)

val nil : atom
(** [nil] constructs a nil pattern. *)

val unit : atom
(** [unit] constructs a unit pattern. *)

val bool : bool -> atom
(** [bool b] constructs a boolean pattern matching [b]. *)

val int : int -> atom
(** [int i] constructs an integer pattern matching [i]. *)

val float : float -> atom
(** [float f] constructs an floating-point pattern matching [f].*)

val string : string -> atom
(** [string s] constructs an string pattern matching [s]. *)

val var : Sym.sym -> atom
(** [var sym] constructs a variable pattern binding the matched value to [sym].
    *)

(** {3 Compound Patterns} *)

val tuple : atom list -> compound
(** [tuple patts] constructs a tuple pattern matching a tuple when the element
    patterns [patts] match their corresponding elements. *)

val cons : atom -> atom -> compound
(** [cons hd tl] constructs a cons pattern matching head and tail pattern [hd]
    and [tl], respectively. *)

val constr : Sym.sym -> atom option -> compound
(** [constr id patt] constructs a variant constructor pattern matching the
    constructor [id] with the optional value pattern [patt]. *)
