open Format

(** {1 Abstract Syntax} *)

type t = private
  | Bool of Loc.t * bool (** Boolean *)
  | Int of Loc.t * int (** Integer *)
  | UnOp of Loc.t * Op.un * t (** Unary Operation *)
  | BinOp of Loc.t * t * Op.bin * t (** Binary Operation *)
  | Let of Loc.t * b * t (** Value Binding *)
  | LetRec of Loc.t * b list * t (** Recursive Value Bindings *)
  | Abs of Loc.t * p list * Type.t * t (** Function Abstraction *)
  | App of Loc.t * t * t list (** Function Application *)
  | Var of Loc.t * string (** Variable Identifier *)
  | If of Loc.t * t * t * t (** Conditional *)
(** Abstract Syntax Tree *)

and b = Loc.t * string * Type.t * t
(** Value Binding *)

and p = Loc.t * string * Type.t
(** Function parameter *)

(** {2 Constructors} *)

val bool : ?loc:Loc.t -> bool -> t
(** [bool ?loc b] constructs a boolean expression with the value [b]. *)

val int : ?loc:Loc.t -> int -> t
(** [int ?loc i] constructs an integer expression with the value [i]. *)

val un_op : ?loc:Loc.t -> Op.un -> t -> t
(** [un_op ?loc op r] constructs a unary operator expression with the operator
    [op] operating on [r]. *)

val bin_op : ?loc:Loc.t -> t -> Op.bin -> t -> t
(** [bin_op ?loc l op r] constructs a binary operator expression with the
    operator [op] operating on [l] and [r]. *)

val bind : ?loc:Loc.t -> b -> t -> t
(** [bind ?loc b rest] constructs a value binding expression binding [b] within
    the scope of [rest]. *)

val bind_rec : ?loc:Loc.t -> b list -> t -> t
(** [bind_rec ?loc bs rest] constructs a set of recursive value bindings over
    the list of bindings [bs] within the scope of both each other and [rest]. *)

val abs : ?loc:Loc.t -> p list -> Type.t -> t -> t
(** [abs ?loc ps ty expr] constructs a function abstraction expression binding a
    parameters [ps] within the scope of the function body [expr] with the result
    type [ty]. *)

val app : ?loc:Loc.t -> t -> t list -> t
(** [app ?loc f xs] constructs a function application expression applying the
    function [f] to the value [x]. *)

val var : ?loc:Loc.t -> string -> t
(** [var ?loc id] constructs a variable identifier expression referencing the
    value bound to the identifier [id]. *)

val cond : ?loc:Loc.t -> t -> t -> t -> t
(** [cond ?loc c t f] constructs a conditional expression branching on the value
    of the expression [c].  If [c] is true, then the expression [t] is
    evaluated.  Otherwise, the expression [f] is evaluated. *)

val binding : ?loc:Loc.t -> string -> Type.t -> t -> b
(** [binding ?loc id ty expr] constructs a value binding expression binding the
    value [expr] of type [ty] to variable identifier [id]. *)

val param : ?loc:Loc.t -> string -> Type.t -> p
(** [param ?loc id ty] constructs a function parameter with the identifier [id]
    bound to values of type [ty]. *)

(** {2 Operations} *)

val pp : t -> formatter -> unit
(** [pp ast fmt] pretty-prints the abstract syntax tree [ast] to the formatter
    [fmt]. *)

val equal : t -> t -> bool
(** [equal ast ast'] tests if two abstract syntax trees [ast] and [ast'] are
    equal.  Note that this is syntactic equality, not sematic equality.  (I.e.,
    [1 + 2] equals [1 + 2], but not [3].) *)

val loc : t -> Loc.t
(** [loc ast] returns the location information associated with the abstract
    syntax tree [ast]. *)

val deloc : t -> t
(** [deloc ast] strips location information from the abstract syntax tree
    [ast]. *)
