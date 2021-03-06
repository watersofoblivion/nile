open Format
open Common

(** {1 Annotated Abstract Syntax}
 *
 * The first intermediate form of for the compiler.  This form is an
 * explicitly-typed lambda calculus with several properties:
 *
 * {ul
 *   {li All files in a single package have been combined into one unit.}
 *   {li All identifiers have been alpha-renamed.}
 *   {li All names (both internal and external to the package) have been
 *       resolved into symbols and import statements have been removed.}
 *   {li Top-level "[def]" and "[val]" statements have been replaced with
 *       "[let]" and "[let rec]" statements.}
 *   {li Unary and Binary operators have been replaced with calls to built-in
 *       functions, except for logical AND and logical OR, which have been
 *       de-sugared into pattern matches to implement short-circuting behavior.}
 *   {li Conditionals ("[if]" statements) have been desugard into pattern
 *       matches over their boolean condition values.}
 *   {li Function abstractions and applications have been uncurried.}
 *   {li Records have been de-sugared into tuples with ascriptions.}
 * }
 *)

(** {2 Syntax} *)

type expr = private
  | Unit                                    (** Unit *)
  | Bool of bool                            (** Boolean *)
  | Int of int                              (** Integer *)
  | Float of float                          (** Floating-point *)
  | String of int * string                  (** String *)
  | Tuple of expr list                      (** Tuples *)
  | Var of Sym.sym                          (** Variable Identifier *)
  | Case of expr * clause list * Type.t     (** Case *)
  | Let of binding * expr                   (** Value Binding *)
  | LetRec of binding list * expr           (** Recursive Value Bindings *)
  | Proj of expr * int                      (** Projection *)
  | Abs of int * param list * Type.t * expr (** Function Abstraction *)
  | App of int * expr * expr list           (** Function Application *)
  | Builtin of Sym.sym * expr list          (** Builtin Function Application *)
(** An expression *)

and param = Patt.t * Type.t
(** A function parameter *)

and binding = Patt.t * Type.t * expr
(** A value Binding *)

and clause = Patt.t * expr
(** A pattern matching clause *)

type top = private
  | TopLet of binding      (** Value binding *)
  | TopRec of binding list (** Recursive value bindings *)
(** A top-level statement *)

type pkg = Sym.sym * top list
(** A source package *)

(** {2 Constructors} *)

(** {3 Expressions} *)

val unit : expr
(** [unit] constructs a unit literal. *)

val bool : bool -> expr
(** [bool b] constructs a boolean literal with the value [b]. *)

val int : int -> expr
(** [int i] constructs an integer literal with the value [i]. *)

val float : float -> expr
(** [float f] constructs a floating-point literal with the value [f]. *)

val string : int -> string -> expr
(** [string len s] constructs a string literal of length [len] with the value
    [s]. *)

val tuple : expr list -> expr
(** [tuple exprs] constructs a tuple literal with the values [exprs]. *)

val var : Sym.sym -> expr
(** [var sym] constructs a variable identifier expression referencing the value
    bound to the symbol [sym]. *)

val case : expr -> clause list -> Type.t -> expr
(** [case scrut clauses res] constructs a case expression scrutinizing [scrut],
    branching to one of [clauses], and resulting in a value of type [res]. *)

val bind : binding -> expr -> expr
(** [bind b rest] constructs a value binding expression binding [b] within the
    scope of [rest]. *)

val bind_rec : binding list -> expr -> expr
(** [bind_rec bs rest] constructs a set of recursive value bindings over the
    list of bindings [bs] within the scope of both each other and [rest]. *)

val proj : expr -> int -> expr
(** [proj expr n] constructs a projection of the [n]th field of [expr]. *)

val abs : int -> param list -> Type.t -> expr -> expr
(** [abs arity params res expr] constructs a function abstraction expression of
    arity [arity] binding the parameters within the scope of the function body
    [expr] with the result type [res]. *)

val app : int -> expr -> expr list -> expr
(** [app arity f xs] constructs a function application expression applying the function
    [f] to the values [xs]. *)

val builtin : Sym.sym -> expr list -> expr
(** [app f xs] constructs a function application expression applying the
    builtin function [f] to the values [xs]. *)

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

(** {3 Packages} *)

val pkg : Sym.sym -> top list -> pkg
(** [pkg name tops] constructs a source package named [name] consisting of the
    list of top-level bindings [tops]. *)

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

val annotate : Type.env -> Ast.file list -> (Type.env * pkg)
(** [annotate env files] type-checks the collection of abstract syntax files
    [files] as a single package in the type environment [env] and returns an
    annotated syntax package along with a type environment with all of the top
    level statements in the package bound. *)

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

val type_of_pkg : Type.env -> pkg -> Type.env
(** [type_of_pkg] type checks the package [pkg] in the type environment [env]
    and returns a type environment with all of the top-level values bound. *)
