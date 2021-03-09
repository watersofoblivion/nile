open Format

(** {1 Pretty Printing}
 *
 * Pretty-prints the ANF IR.
 *)

(** {2 Types} *)

val ty : Sym.names -> Type.t -> formatter -> unit
(** [ty names ty fmt] pretty-prints the type [ty] to the formatter [fmt]
    resolving symbols with [names]. *)

(** {2 Patterns} *)

val atom_patt : Sym.names -> Patt.atom -> formatter -> unit
(** [atom_patt names patt fmt] pretty-prints the atomic pattern [patt] to the
    formatter [fmt] resolving symbols with [names]. *)

val compound_patt : Sym.names -> Patt.compound -> formatter -> unit
(** [compound_patt names patt fmt] pretty-prints the compound pattern [patt] to
    the formatter [fmt] resolving symbols with [names]. *)

(** {2 Abstract Syntax Trees} *)

val atom : Sym.names -> Anf.atom -> formatter -> unit
(** [atom names a fmt] pretty-prints the atom [a] to the formatter [fmt]
    resolving symbols with [names]. *)

val expr : Sym.names -> Anf.expr -> formatter -> unit
(** [expr names e fmt] pretty-prints the expression [e] to the formatter [fmt]
    resolving symbols with [names]. *)

val block : Sym.names -> Anf.block -> formatter -> unit
(** [block names b fmt] pretty-prints the block [b] to the formatter [fmt]
    resolving symbols with [names]. *)

val top : Sym.names -> Ast.top -> formatter -> unit
(** [top names t fmt] pretty-prints the top-level binding [t] to the formatter
    [fmt] resolving symbols with [names]. *)

val pkg : Sym.names -> Ast.pkg -> formatter -> unit
(** [pkg names p fmt] pretty-prints the package [p] to the formatter [fmt]
    resolving symbols with [names]. *)
