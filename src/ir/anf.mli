open Format
open Common

(** {1 Administrative Normal Form Intermediate Representation} *)

(** {2 Syntax} *)

type atom = private
  | Unit                                    (** Unit *)
  | Bool of bool                            (** Boolean *)
  | Int of int                              (** Integer *)
  | Var of Sym.sym                          (** Variable *)
  | Abs of Patt.t * Type.t * Type.t * block (** Function Abstraction *)
(** Atomic Values *)

and expr = private
  | UnOp of Op.un * atom          (** Unary Operator *)
  | BinOp of atom * Op.bin * atom (** Binary Operator *)
  | App of atom * atom            (** Function Application *)
  | Atom of atom                  (** Atomic Value *)
(** Primitive Expressions *)

and block = private
  | Let of binding * block         (** Value Binding *)
  | LetRec of binding list * block (** Recursive Value Bindings *)
  | Case of atom * case list       (** Case *)
  | Expr of expr                   (** Primitive Expression *)
(** Block Expressions *)

and binding = Patt.t * Type.t * expr
(** Bound Value *)

and case = Patt.t * block
(** Match Case *)

type top = private
  | TopLet of binding      (** Value binding *)
  | TopRec of binding list (** Recursive value bindings *)
(** A top-level statement *)

type file = top list
(** A source file *)

(** {2 Constructors} *)

(** {3 Atomic Values} *)

val unit : atom
(** [unit] constructs an atomic unit literal. *)

val bool : bool -> atom
(** [bool b] constructs an atomic boolean literal with the value [b]. *)

val int : int -> atom
(** [int i] constructs an atomic integer literal with the value [i]. *)

val var : Sym.sym -> atom
(** [var sym] constructs an atomic variable referencing the bound value [sym]. *)

val abs : Patt.t -> Type.t -> Type.t -> block -> atom
(** [abs patt ty res body] constructs an atomic function abstraction binding the
    pattern [patt] of type [ty] within [body] and resulting in values of type
    [res]. *)

(** {3 Primitive Expressions} *)

val un_op : Op.un -> atom -> expr
(** [un_op op r] constructs a unary operation by applying the unary operator
    [op] to the atomic value [r]. *)

val bin_op : atom -> Op.bin -> atom -> expr
(** [bin_op l op r] constructs a binary operator expression by applying the
    binary operator [op] to the atomic values [l] and [r]. *)

val app : atom -> atom -> expr
(** [app f x] constructs a function application expression by applying the
    function [f] to the atomic value [x]. *)

val atom : atom -> expr
(** [atom a] constructs a primitive expression representing the atomic value
    [a]. *)

(** {3 Block Expressions} *)

val bind : binding -> block -> block
(** [bind b rest] constructs a variable binding expression binding [b] within
    [rest]. *)

val bind_rec : binding list -> block -> block
(** [bind_rec bs rest] constructs a recursive variable binding expression
    binding all of [bs] within each other and within [rest]. *)

val case_of : atom -> case list -> block
(** [case_of scrut cases] constructs a case of expression branching depending on
    the value of the atomic expression [scrut]. *)

val expr : expr -> block
(** [expr e] constructs a block expression representing the primitive expression
    [e]. *)

(** {3 Variable Bindings} *)

val binding : Patt.t -> Type.t -> expr -> binding
(** [binding patt ty expr] constructs a variable binding which binds the pattern
    [patt] of type [ty] to the value of [expr]. *)

(** {3 Match Cases} *)

val case : Patt.t -> block -> case
(** [case patt body] constructs a pattern matching case which binds the pattern
    [patt] in [body]. *)

(** {3 Top-Level Statements} *)

val top_bind : binding -> top
(** [top_bind b] constructs a value binding expression binding [b]. *)

val top_bind_rec : binding list -> top
(** [top_bind_rec bs rest] constructs a set of recursive top-level value
    bindings over the list of bindings [bs]. *)

(** {3 Files} *)

val file : top list -> file
(** [file tops] constructs a source file consisting of the list of top-level
    bindings [tops]. *)

(** {2 Pretty Printing} *)

val pp_atom : Sym.names -> atom -> formatter -> unit
(** [pp_atom names atom fmt] pretty-prints the atomic value [atom] to the
    formatter [fmt] using the names from the symbol table [names]. *)

val pp_expr : Sym.names -> expr -> formatter -> unit
(** [pp_expr names expr fmt] pretty-prints the primitive expression [expr] to
    the formatter [fmt] using the names from the symbol table [names]. *)

val pp_block : Sym.names -> block -> formatter -> unit
(** [pp_block names block fmt] pretty-prints the block expression [block] to the
    formatter [fmt] using the names from the symbol table [names]. *)

val pp_top : Sym.names -> top -> formatter -> unit
(** [pp_top names top fmt] pretty-prints the top-level expression [top] to the
    formatter [fmt] using the names from the symbol table [names]. *)

val pp_file : Sym.names -> file -> formatter -> unit
(** [pp_file names file fmt] pretty-prints the source file [file] to the
    formatter [fmt] using the names from the symbol table [names]. *)

(** {2 Type Checking} *)

val builtin : Check.env
(** [builtin] returns a type-checking environment with the builtin functions
    bound. *)

val type_of_atom : Check.env -> atom -> Type.t
(** [type_of_atom env atom] type checks the atomic value [atom] in the type
    environment [env] and results in the type of the atomic value. *)

val type_of_expr : Check.env -> expr -> Type.t
(** [type_of_expr env expr] type checks the primitive expression [expr] in the
    type environment [env] and results in the type of the primitive expression.
    *)

val type_of_block : Check.env -> block -> Type.t
(** [type_of_block env block] type checks the block expression [block] in the
    type environment [env] and results in the type of the block expression. *)

val type_of_top : Check.env -> top -> Check.env
(** [type_of_top env top] type checks the top-level expression [top] in the
    type environment [env] and results in a type environment with the top-level
    identifiers bound to their types. *)

val type_of_file : Check.env -> file -> Check.env
(** [type_of_file env file] type checks the source file [file] in the type
    environment [env] and returns a type environment with all top-level
    identifiers bound to their types. *)
