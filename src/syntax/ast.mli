open Format

(** {1 Abstract Syntax} *)

(** {2 Expressions} *)

type expr = private
  | Bool of Loc.t * bool (** Boolean *)
  | Int of Loc.t * int (** Integer *)
  | Var of Loc.t * string (** Variable Identifier *)
  | UnOp of Loc.t * Op.un * expr (** Unary Operation *)
  | BinOp of Loc.t * expr * Op.bin * expr (** Binary Operation *)
  | If of Loc.t * expr * expr * expr (** Conditional *)
  | Let of Loc.t * binding * expr (** Value Binding *)
  | LetRec of Loc.t * binding list * expr (** Recursive Value Bindings *)
  | Abs of Loc.t * param list * Type.t * expr (** Function Abstraction *)
  | App of Loc.t * expr * expr list (** Function Application *)
(** An expression *)

and binding = Loc.t * string * Type.t * expr
(** Value Binding *)

and param = Loc.t * string * Type.t
(** Function parameter *)

(** {3 Constructors} *)

val bool : Loc.t -> bool -> expr
(** [bool loc b] constructs a boolean expression with the value [b]. *)

val int : Loc.t -> int -> expr
(** [int loc i] constructs an integer expression with the value [i]. *)

val var : Loc.t -> string -> expr
(** [var loc id] constructs a variable identifier expression referencing the
    value bound to the identifier [id]. *)

val un_op : Loc.t -> Op.un -> expr -> expr
(** [un_op loc op r] constructs a unary operator expression with the operator
    [op] operating on [r]. *)

val bin_op : Loc.t -> expr -> Op.bin -> expr -> expr
(** [bin_op loc l op r] constructs a binary operator expression with the
    operator [op] operating on [l] and [r]. *)

val cond : Loc.t -> expr -> expr -> expr -> expr
(** [cond loc c t f] constructs a conditional expression branching on the value
    of the expression [c].  If [c] is true, then the expression [t] is
    evaluated.  Otherwise, the expression [f] is evaluated. *)

val bind : Loc.t -> binding -> expr -> expr
(** [bind loc b rest] constructs a value binding expression binding [b] within
    the scope of [rest]. *)

val bind_rec : Loc.t -> binding list -> expr -> expr
(** [bind_rec loc bs rest] constructs a set of recursive value bindings over the
    list of bindings [bs] within the scope of both each other and [rest]. *)

val abs : Loc.t -> param list -> Type.t -> expr -> expr
(** [abs loc ps ty expr] constructs a function abstraction expression binding a
    parameters [ps] within the scope of the function body [expr] with the result
    type [ty]. *)

val app : Loc.t -> expr -> expr list -> expr
(** [app loc f xs] constructs a function application expression applying the
    function [f] to the value [x]. *)

val binding : Loc.t -> string -> Type.t -> expr -> binding
(** [binding loc id ty expr] constructs a value binding expression binding the
    value [expr] of type [ty] to variable identifier [id]. *)

val param : Loc.t -> string -> Type.t -> param
(** [param loc id ty] constructs a function parameter with the identifier [id]
    bound to values of type [ty]. *)

(** {3 Operations} *)

val pp_expr : expr -> formatter -> unit
(** [pp_expr expr fmt] pretty-prints the expression [expr] to the formatter
    [fmt]. *)

val loc_expr : expr -> Loc.t
(** [loc_expr expr] returns the location information associated with the
    expression [expr]. *)

(** {2 Top-level Statements} *)

type top = private
  | TopLet of Loc.t * binding      (** Value binding *)
  | TopRec of Loc.t * binding list (** Recursive value bindings *)
(** A top-level statement *)

(** {3 Constructors} *)

val top_bind : Loc.t -> binding -> top
(** [top_bind loc b] constructs a value binding expression binding [b]. *)

val top_bind_rec : Loc.t -> binding list -> top
(** [top_bind_rec loc bs rest] constructs a set of recursive top-level value
    bindings over the list of bindings [bs]. *)

(** {3 Operations} *)

val pp_top : top -> formatter -> unit
(** [pp_top top fmt] pretty-prints the top-level binding [top] to the formatter
    [fmt]. *)

val loc_top : top -> Loc.t
(** [loc_top expr] returns the location information associated with the
    top-level binding [top]. *)

(** {2 Source Files} *)

type file = top list
(** A source file *)

(** {3 Constructors} *)

val file : top list -> file
(** [file tops] constructs a source file consisting of the list of top-level
    bindings [tops]. *)

(** {3 Operations} *)

val pp_file : file -> formatter -> unit
(** [pp_file file fmt] pretty-prints the file [file] to the formatter [fmt]. *)
