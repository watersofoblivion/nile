open Format
open Common

(** {1 Administrative Normal Form Intermediate Representation} *)

(** {2 Syntax} *)

type atom = private
  | Bool of bool                         (** Boolean *)
  | Int of int                           (** Integer *)
  | Var of int                           (** Variable *)
  | Abs of int * Type.t * Type.t * block (** Function Abstraction *)
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
  | If of atom * block * block     (** Conditional *)
  | Expr of expr                   (** Primitive Expression *)
(** Block Expressions *)

and binding = int * Type.t * expr
(** Bound Value *)

type top = private
  | TopLet of binding      (** Value binding *)
  | TopRec of binding list (** Recursive value bindings *)
(** A top-level statement *)

type file = top list
(** A source file *)

(** {2 Constructors} *)

(** {3 Atomic Values} *)

val bool : bool -> atom
(** [bool b] constructs an atomic boolean literal with the value [b]. *)

val int : int -> atom
(** [int i] constructs an atomic integer literal with the value [i]. *)

val var : int -> atom
(** [var idx] constructs an atomic variable referencing the bound value [idx]. *)

val abs : int -> Type.t -> Type.t -> block -> atom
(** [abs idx ty res body] constructs an atomic function abstraction binding the
    variable [idx] of type [ty] within [body] and resulting in values of type
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

val cond : atom -> block -> block -> block
(** [cond c t f] constructs a conditional expression branching to either [t] or
    [f] depending on the value of the atomic expression [c]. *)

val expr : expr -> block
(** [expr e] constructs a block expression representing the primitive expression
    [e]. *)

(** {3 Variable Bindings} *)

val binding : int -> Type.t -> expr -> binding
(** [binding idx ty expr] constructs a variable binding which binds the variable
    [idx] of type [ty] to the value of [expr]. *)

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

(** {2 Normalization} *)

val builtin_idx : int
(** [builtin_idx] is the alpha index after binding builtin functions. *)

val builtin_aenv : (string * int) list
(** [builtin_aenv] is the alpha environment with the builtin functions bound. *)

val builtin_tenv : (int * Type.t) list
(** [builtin_tenv] is the type environment with the builtin functions bound. *)

val of_expr : int -> (string * int) list -> (int * Type.t) list -> (int * Type.t) option -> Annot.expr -> (int * Type.t * block)
(** [of_expr idx aenv tenv join expr] normalizes the annotated abstract syntax
    tree expression [expr] into administrative normal form.

    All bound identifiers are alpha renamed to indicies, starting with [idx].
    Previously bound names are mapped in [aenv], and the types of bound values
    are mapped in [tenv].

    The value [join] represents an optional join point for the normalization to
    call, consisting of the index of the variable the join point is bound to and
    the result type of the join point.

    The function results in a triple of the next alpha renaming index, the type
    of the normalized expression, and the normalized expression itself.  All
    normalized expression are packed into a block expression by wrapping them
    with {!Anf.Expr} and/or {!Anf.Atom} nodes. *)

val of_top : int -> (string * int) list -> (int * Type.t) list -> Annot.top -> (int * (string * int) list * (int * Type.t) list * top list)
(** [of_top idx aenv tenv top] normalizes the annotated abstract syntax tree
    top-level expression [expr] into administrative normal form.  (See
    {!of_expr} for details.)

    Returns a quadruple of the next alpha renaming index, an alpha and a type
    environment with the top-level values bound, and a list of normalized
    top-level expressions.  The expressions are returned in dependency order. *)

val of_file : int -> (string * int) list -> (int * Type.t) list -> Annot.file -> (int * file)
(** [of_file idx aenv tenv file] normalizes the annotated abstract syntax tree
    file [file] into administrative normal form.  (See {!of_expr} for details.)

    Returns a pair of the next alpha renaming index and a normalized file. *)

(** {2 Pretty Printing} *)

val pp_atom : atom -> formatter -> unit
(** [pp_atom atom fmt] pretty-prints the atomic value [atom] to the formatter
    [fmt]. *)

val pp_expr : expr -> formatter -> unit
(** [pp_expr expr fmt] pretty-prints the primitive expression [expr] to the
    formatter [fmt]. *)

val pp_block : block -> formatter -> unit
(** [pp_block block fmt] pretty-prints the block expression [block] to the
    formatter [fmt]. *)

val pp_top : top -> formatter -> unit
(** [pp_top top fmt] pretty-prints the top-level expression [top] to the
    formatter [fmt]. *)

val pp_file : file -> formatter -> unit
(** [pp_file file fmt] pretty-prints the source file [file] to the formatter
    [fmt]. *)

(** {2 Type Checking} *)

type env
(** A type-checking environment *)

val env : env
(** [env] returns an empty type-checking environment *)

val builtin : env
(** [builtin] returns a type-checking environment with the builtin functions
    bound. *)

val type_of_atom : env -> atom -> Type.t
(** [type_of_atom env atom] type checks the atomic value [atom] in the type
    environment [env] and results in the type of the atomic value. *)

val type_of_expr : env -> expr -> Type.t
(** [type_of_expr env expr] type checks the primitive expression [expr] in the
    type environment [env] and results in the type of the primitive expression.
    *)

val type_of_block : env -> block -> Type.t
(** [type_of_block env block] type checks the block expression [block] in the
    type environment [env] and results in the type of the block expression. *)

val type_of_top : env -> top -> env
(** [type_of_top env top] type checks the top-level expression [top] in the
    type environment [env] and results in a type environment with the top-level
    identifiers bound to their types. *)

val type_of_file : env -> file -> env
(** [type_of_file env file] type checks the source file [file] in the type
    environment [env] and returns a type environment with all top-level
    identifiers bound to their types. *)
