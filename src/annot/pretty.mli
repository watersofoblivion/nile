open Format

(** {1 Pretty Printing}
 *
 * Pretty-prints the annotated syntax.
 *)

(** {2 Types} *)

val ty : Sym.names -> Type.t -> formatter -> unit
(** [ty names ty fmt] pretty-prints the type [ty] to the formatter [fmt]
    resolving symbols with [names]. *)

(** {2 Patterns} *)

val patt : Sym.names -> Patt.t -> formatter -> unit
(** [patt names patt fmt] pretty-prints the pattern [patt] to the formatter
    [fmt] resolving symbols with [names]. *)

(** {2 Operators} *)

val un : Op.un -> formatter -> unit
(** [un op fmt] pretty-prints the unary operator [op] to the formatter [fmt]. *)

val bin : Op.bin -> formatter -> unit
(** [bin op fmt] pretty-prints the binary operator [op] to the formatter [fmt].
   *)

(** {2 Abstract Syntax Trees} *)

val expr : Sym.names -> Ast.expr -> formatter -> unit
(** [expr names e fmt] pretty-prints the expression [e] to the formatter [fmt]
    resolving symbols with [names]. *)

val top : Sym.names -> Ast.top -> formatter -> unit
(** [top names t fmt] pretty-prints the top-level binding [t] to the formatter
    [fmt] resolving symbols with [names]. *)

val pkg : Sym.names -> Ast.pkg -> formatter -> unit
(** [pkg names p fmt] pretty-prints the package [p] to the formatter [fmt]
    resolving symbols with [names]. *)
