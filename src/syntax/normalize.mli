open Ir

(** {1 A-Normalization} *)

val builtin_idx : int
(** [builtin_idx] is the alpha index after binding builtin functions. *)

val builtin_aenv : (int * int) list
(** [builtin_aenv] is the alpha environment with the builtin functions bound. *)

val builtin_tenv : (int * Type.t) list
(** [builtin_tenv] is the type environment with the builtin functions bound. *)

val of_expr : int -> (int * int) list -> (int * Type.t) list -> (int * Type.t) option -> Ast.expr -> (int * Type.t * Anf.block)
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

val of_top : int -> (int * int) list -> (int * Type.t) list -> Ast.top -> (int * (int * int) list * (int * Type.t) list * Anf.top list)
(** [of_top idx aenv tenv top] normalizes the annotated abstract syntax tree
    top-level expression [expr] into administrative normal form.  (See
    {!of_expr} for details.)

    Returns a quadruple of the next alpha renaming index, an alpha and a type
    environment with the top-level values bound, and a list of normalized
    top-level expressions.  The expressions are returned in dependency order. *)

val of_file : int -> (int * int) list -> (int * Type.t) list -> Ast.file -> (int * Anf.file)
(** [of_file idx aenv tenv file] normalizes the annotated abstract syntax tree
    file [file] into administrative normal form.  (See {!of_expr} for details.)

    Returns a pair of the next alpha renaming index and a normalized file. *)
