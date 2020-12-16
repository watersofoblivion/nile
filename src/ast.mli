open Format

(** {1 Abstract Syntax} *)

type t = private
  | Bool of Loc.t * bool (** Boolean *)
  | Int of Loc.t * int (** Integer *)
  | Var of Loc.t * string (** Variable Identifier *)
  | UnOp of Loc.t * Op.un * t (** Unary Operation *)
  | BinOp of Loc.t * t * Op.bin * t (** Binary Operation *)
  | If of Loc.t * t * t * t (** Conditional *)
  | Let of Loc.t * b * t (** Value Binding *)
  | LetRec of Loc.t * b list * t (** Recursive Value Bindings *)
  | Abs of Loc.t * p list * Type.t * t (** Function Abstraction *)
  | App of Loc.t * t * t list (** Function Application *)
(** Abstract Syntax Tree *)

and b = Loc.t * string * Type.t * t
(** Value Binding *)

and p = Loc.t * string * Type.t
(** Function parameter *)

(** {2 Constructors} *)

val bool : Loc.t -> bool -> t
(** [bool loc b] constructs a boolean expression with the value [b]. *)

val int : Loc.t -> int -> t
(** [int loc i] constructs an integer expression with the value [i]. *)

val var : Loc.t -> string -> t
(** [var loc id] constructs a variable identifier expression referencing the
    value bound to the identifier [id]. *)

val un_op : Loc.t -> Op.un -> t -> t
(** [un_op loc op r] constructs a unary operator expression with the operator
    [op] operating on [r]. *)

val bin_op : Loc.t -> t -> Op.bin -> t -> t
(** [bin_op loc l op r] constructs a binary operator expression with the
    operator [op] operating on [l] and [r]. *)

val cond : Loc.t -> t -> t -> t -> t
(** [cond loc c t f] constructs a conditional expression branching on the value
    of the expression [c].  If [c] is true, then the expression [t] is
    evaluated.  Otherwise, the expression [f] is evaluated. *)

val bind : Loc.t -> b -> t -> t
(** [bind loc b rest] constructs a value binding expression binding [b] within
    the scope of [rest]. *)

val bind_rec : Loc.t -> b list -> t -> t
(** [bind_rec loc bs rest] constructs a set of recursive value bindings over the
    list of bindings [bs] within the scope of both each other and [rest]. *)

val abs : Loc.t -> p list -> Type.t -> t -> t
(** [abs loc ps ty expr] constructs a function abstraction expression binding a
    parameters [ps] within the scope of the function body [expr] with the result
    type [ty]. *)

val app : Loc.t -> t -> t list -> t
(** [app loc f xs] constructs a function application expression applying the
    function [f] to the value [x]. *)

val binding : Loc.t -> string -> Type.t -> t -> b
(** [binding loc id ty expr] constructs a value binding expression binding the
    value [expr] of type [ty] to variable identifier [id]. *)

val param : Loc.t -> string -> Type.t -> p
(** [param loc id ty] constructs a function parameter with the identifier [id]
    bound to values of type [ty]. *)

(** {2 Operations} *)

val pp : t -> formatter -> unit
(** [pp ast fmt] pretty-prints the abstract syntax tree [ast] to the formatter
    [fmt]. *)

val loc : t -> Loc.t
(** [loc ast] returns the location information associated with [ast]. *)
