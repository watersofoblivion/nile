(** {1 Type Checking}
 *
 * Type checks the annotated source.
 *)

(** {2 Patterns} *)

val irrefutable : Patt.t -> bool
(** [irrefutable patt] tests whether or not the pattern [patt] is irrefutable.
    That is, whether it will match all values of a type [ty] for which
    [matches_type patt ty] returns [true]. *)

(** {2 Abstract Syntax} *)

val type_of_expr : Type.env -> Ast.expr -> Type.t
(** [type_of_expr env expr] type checks the expression [expr] in the type
    environment [env] and returns its type. *)

val type_of_top : Type.env -> Ast.top -> Type.env
(** [type_of_top env top] type checks the top-level expression [top] in the type
    environment [env] and returns its type along with a type environment with
    the top-level value bound. *)

val type_of_pkg : Type.env -> Ast.pkg -> Type.env
(** [type_of_pkg] type checks the package [pkg] in the type environment [env]
    and returns a type environment with all of the top-level values bound. *)
