open Format
open Common

(** {1 Unannotated Abstract Syntax} *)

(** {2 Symbolization} *)

type sym
(** A symbol *)

type tbl
(** A symbol table *)

val tbl : tbl
(** [tbl] returns an empty symbol table *)

val symbolize : string -> tbl -> (sym * tbl)
(** [symbolize str tbl] converts [str] into a symbol from the table [tbl].  If
    [str] has already been symbolized, the same symbol and an unaltered table
    are returned.  Otherwise, a new symbol is generated and returned along with
    a copy of the table with the symbol bound. *)

val name_of : sym -> tbl -> string
(** [name_of sym tbl] looks up the string representation of the symbol [sym] in
    the table [tbl].  Raises {!Not_found} if [sym] does not exist in [tbl]. *)

(** {2 Syntax} *)

type expr = private
  | Bool of Loc.t * bool                               (** Boolean *)
  | Int of Loc.t * int                                 (** Integer *)
  | Var of Loc.t * sym                                 (** Variable Identifier *)
  | UnOp of Loc.t * Op.un * expr                       (** Unary Operation *)
  | BinOp of Loc.t * expr * Op.bin * expr              (** Binary Operation *)
  | If of Loc.t * expr * expr * expr                   (** Conditional *)
  | Let of Loc.t * binding * expr                      (** Value Binding *)
  | LetRec of Loc.t * binding list * expr              (** Recursive Value Bindings *)
  | Abs of Loc.t * sym * Type.t * Type.t option * expr (** Function Abstraction *)
  | App of Loc.t * expr * expr                         (** Function Application *)
(** An expression *)

and binding = Loc.t * sym * Type.t option * expr
(** A value Binding *)

type top = private
  | TopLet of Loc.t * binding      (** Value binding *)
  | TopRec of Loc.t * binding list (** Recursive value bindings *)
(** A top-level statement *)

type file = top list
(** A source file *)

(** {2 Constructors} *)

(** {3 Expressions} *)

val bool : Loc.t -> bool -> expr
(** [bool loc b] constructs a boolean literal with the value [b] at location
    [loc]. *)

val int : Loc.t -> int -> expr
(** [int loc i] constructs an integer literal with the value [i] at location
    [loc]. *)

val var : Loc.t -> sym -> expr
(** [var loc id] constructs a variable identifier expression at location [loc]
    referencing the value bound to the identifier [id]. *)

val un_op : Loc.t -> Op.un -> expr -> expr
(** [un_op loc op r] constructs a unary operator expression at location [loc]
    with the operator [op] operating on [r].  The location should span from [op]
    to [r]. *)

val bin_op : Loc.t -> expr -> Op.bin -> expr -> expr
(** [bin_op loc l op r] constructs a binary operator expression at location
    [loc] with the operator [op] operating on [l] and [r].  The location should
    span from [l] to [r]. *)

val cond : Loc.t -> expr -> expr -> expr -> expr
(** [cond loc c t f] constructs a conditional expression at location [loc]
    branching on the value of the expression [c].  If [c] is true, then the
    expression [t] is evaluated.  Otherwise, the expression [f] is evaluated.
    The location should span from the initial "if" keyword to the end of [f]. *)

val bind : Loc.t -> binding -> expr -> expr
(** [bind loc b rest] constructs a value binding expression at location [loc]
    binding [b] within the scope of [rest].  The location should span from the
    initial "let" keyword to the end of the [rest] expression. *)

val bind_rec : Loc.t -> binding list -> expr -> expr
(** [bind_rec loc bs rest] constructs a set of recursive value bindings at the
    location [loc] over the list of bindings [bs] within the scope of both each
    other and [rest].  The location should span from the initial "let" keyword
    to the end of the [rest] expression. *)

val abs : Loc.t -> sym -> Type.t -> Type.t option -> expr -> expr
(** [abs loc id ty res expr] constructs a function abstraction expression at
    location [loc] binding the parameter named [id] with type [ty] within the
    scope of the function body [expr] with the result type [res].  The location
    should span from the [id] to the end of [body].  If not provided, the type
    [res] will be inferred. *)

val app : Loc.t -> expr -> expr -> expr
(** [app loc f x] constructs a function application expression at location [loc]
    applying the function [f] to the value [x].  The location should span from
    [f] to [x]. *)

val binding : Loc.t -> sym -> Type.t option -> expr -> binding
(** [binding loc id ty expr] constructs a value binding expression at location
    [loc] binding the value [expr] of type [ty] to variable identifier [id].
    The location should span from [id] to [expr].  If not provided, the type
    [ty] will be inferred. *)

(** {3 Top-Level Statements} *)

val top_bind : Loc.t -> binding -> top
(** [top_bind loc b] constructs a value binding expression at location [loc]
    binding [b].  The location should span from the initial "let" keyword to
    [b]. *)

val top_bind_rec : Loc.t -> binding list -> top
(** [top_bind_rec loc bs rest] constructs a set of recursive top-level value
    bindings at location [loc] over the list of bindings [bs].  The location
    should span from the initial "let" keyword to the last binding of [bs]. *)

(** {3 Files} *)

val file : top list -> file
(** [file tops] constructs a source file consisting of the list of top-level
    bindings [tops]. *)

(** {2 Operations} *)

val precedence : expr -> int
(** [precedence expr] returns the precedence of the express [expr]. *)

val loc_expr : expr -> Loc.t
(** [loc_expr expr] returns the location information associated with the
    expression [expr]. *)

val loc_top : top -> Loc.t
(** [loc_top expr] returns the location information associated with the
    top-level binding [top]. *)

(** {2 Pretty Printing} *)

val pp_expr : tbl -> expr -> formatter -> unit
(** [pp_expr tbl expr fmt] pretty-prints the expression [expr] to the formatter
    [fmt] converting symbols to strings using [tbl]. *)

val pp_top : tbl -> top -> formatter -> unit
(** [pp_top top fmt] pretty-prints the top-level binding [top] to the formatter
    [fmt] converting symbols to strings using [tbl]. *)

val pp_file : tbl -> file -> formatter -> unit
(** [pp_file file fmt] pretty-prints the file [file] to the formatter [fmt]
     converting symbols to strings using [tbl]. *)
