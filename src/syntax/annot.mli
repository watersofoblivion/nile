open Format
open Common

(** {1 Annotated Abstract Syntax} *)

(** {2 Syntax} *)

type expr = private
  | Unit                                   (** Unit *)
  | Bool of bool                           (** Boolean *)
  | Int of int                             (** Integer *)
  | Var of Sym.sym                         (** Variable Identifier *)
  | UnOp of Op.un * expr                   (** Unary Operation *)
  | BinOp of expr * Op.bin * expr          (** Binary Operation *)
  | Case of expr * clause list * Type.t    (** Case Of *)
  | Let of binding * expr                  (** Value Binding *)
  | LetRec of binding list * expr          (** Recursive Value Bindings *)
  | Abs of Patt.t * Type.t * Type.t * expr (** Function Abstraction *)
  | App of expr * expr                     (** Function Application *)
(** An expression *)

and binding = Patt.t * Type.t * expr
(** A value Binding *)

and clause = Patt.t * expr
(** A pattern matching clause *)

type top = private
  | TopLet of binding      (** Value binding *)
  | TopRec of binding list (** Recursive value bindings *)
(** A top-level statement *)

type file = top list
(** A source file *)

(** {2 Constructors} *)

(** {3 Expressions} *)

val unit : expr
(** [unit] constructs a unit literal. *)

val bool : bool -> expr
(** [bool b] constructs a boolean literal with the value [b]. *)

val int : int -> expr
(** [int i] constructs an integer literal with the value [i]. *)

val var : Sym.sym -> expr
(** [var sym] constructs a variable identifier expression referencing the value
    bound to the symbol [sym]. *)

val un_op : Op.un -> expr -> expr
(** [un_op op r] constructs a unary operator expression with the operator [op]
    operating on [r]. *)

val bin_op : expr -> Op.bin -> expr -> expr
(** [bin_op l op r] constructs a binary operator expression with the operator
    [op] operating on [l] and [r]. *)

val case : expr -> clause list -> Type.t -> expr
(** [case scrut clauses res] constructs a case expression scrutinizing [scrut],
    branching to one of [clauses], and resulting in a value of type [res]. *)

val bind : binding -> expr -> expr
(** [bind b rest] constructs a value binding expression binding [b] within the
    scope of [rest]. *)

val bind_rec : binding list -> expr -> expr
(** [bind_rec bs rest] constructs a set of recursive value bindings over the
    list of bindings [bs] within the scope of both each other and [rest]. *)

val abs : Patt.t -> Type.t -> Type.t -> expr -> expr
(** [abs patt ty res expr] constructs a function abstraction expression binding
    the parameter pattern [patt] with type [ty] within the scope of the function
    body [expr] with the result type [res]. *)

val app : expr -> expr -> expr
(** [app f x] constructs a function application expression applying the function
    [f] to the value [x]. *)

val binding : Patt.t -> Type.t -> expr -> binding
(** [binding patt ty expr] constructs a value binding expression binding the
    value [expr] of type [ty] to pattern [patt]. *)

val clause : Patt.t -> expr -> clause
(** [clause patt expr] constructs a pattern matching clause that executes [expr]
    when [patt] is matched. *)

(** {3 Top-Level Statements} *)

val top_bind : binding -> top
(** [top_bind b] constructs a top-level value binding expression binding [b]. *)

val top_bind_rec : binding list -> top
(** [top_bind_rec bs rest] constructs a set of recursive top-level value
    bindings over the list of bindings [bs]. *)

(** {3 Files} *)

val file : top list -> file
(** [file tops] constructs a source file consisting of the list of top-level
    bindings [tops]. *)

(** {2 Annotation} *)

val annotate_expr : Type.env -> Ast.expr -> expr
(** [annotate_expr env expr] type-checks the abstract syntax expression [expr]
    in the type environment [env] and returns an annotated syntax expression. *)

val annotate_top : Type.env -> Ast.top -> (Type.env * top)
(** [annotate_top env top] type-checks the abstract syntax top-level statement
    [top] in the type environment [env] and returns an annotated syntax
    top-level statement along with a type environment with the top-level
    statement bound. *)

val annotate_file : Type.env -> Ast.file -> (Type.env * file)
(** [annotate_file env file] type-checks the abstract syntax file [file] in the
    type environment [env] and returns an annotated syntax file along with a
    type environment with all of the top-level statements in the file bound. *)

(** {2 Operations} *)

val precedence : expr -> int
(** [precedence expr] returns the precedence of the express [expr]. *)

(** {2 Pretty Printing} *)

val pp_expr : Sym.names -> expr -> formatter -> unit
(** [pp_expr names expr fmt] pretty-prints the expression [expr] to the
    formatter [fmt] converting symbols to strings using [names]. *)

val pp_top : Sym.names -> top -> formatter -> unit
(** [pp_top names top fmt] pretty-prints the top-level binding [top] to the
    formatter [fmt] converting symbols to strings using [names]. *)

val pp_file : Sym.names -> file -> formatter -> unit
(** [pp_file names file fmt] pretty-prints the file [file] to the formatter
    [fmt] converting symbols to strings using [names]. *)

(** {2 Type Checking} *)

val type_of_expr : Type.env -> expr -> Type.t
(** [type_of_expr env expr] type checks the expression [expr] in the type
    environment [env] and returns its type. *)

val type_of_top : Type.env -> top -> Type.env
(** [type_of_top env top] type checks the top-level expression [top] in the type
    environment [env] and returns its type along with a type environment with
    the top-level value bound. *)

val type_of_file : Type.env -> file -> Type.env
(** [type_of_file] type checks the file [file] in the type environment [env] and
    returns a type environment with all of the top-level values bound. *)
