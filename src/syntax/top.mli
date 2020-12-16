open Format

(** {1 Toplevel Statements} *)

module type Lang =
  sig
    type t
    (** Expressions which can be bound in this language. *)

    type p = Loc.t * string * Type.t
    (** A parameter to a function. *)

    val is_abs : t -> (p list * Type.t * t) option
    (** [is_abs expr] tests if the expression [expr] is a function abstraction.
        If it is, this returns the parameter list, result type, and body of the
        abstraction.  Otherwise, it returns [None]. *)

    val pp : t -> formatter -> unit
    (** [pp expr fmt] pretty-prints the expression [expr] in this language to
        the formatter [fmt]. *)
  end
(** The signature of a module containing a language.  All representations of the
    program (Abstract Syntax, Intermediate Representation, Closure-converted)
    are languages which can have top-level statements, and so implement this
    signature. *)

module type Top =
  sig
    type e
    (** The type of expressions in a source language. *)

    type b = Loc.t * string * Type.t * e
    (** A value binding *)

    type t = private
      | Let of Loc.t * b (** Value binding *)
      | LetRec of Loc.t * b list (** Recursive value bindings *)
    (** Top-level statements *)

    val bind : Loc.t -> b -> t
    (** [bind loc b] constructs a value binding statement binding the value
        [b]. *)

    val bind_rec : Loc.t -> b list -> t
    (** [bind_rec loc bs] constructs a recursive value binding statement binding
        the values in [bs]. *)

    val pp : t -> formatter -> unit
    (** [pp top fmt] pretty-prints the top-level statement [top] to the
        formatter [fmt]. *)
  end
(** The common signature of all modules for working with top-level statements in
    a particular language. *)

module Ast: Top with type e = Ast.t
(** Top-level bindings for the Abstract Syntax. *)
