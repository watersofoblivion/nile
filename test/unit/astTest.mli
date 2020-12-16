open OUnit2
open Nile

(** {1 Abstract Syntax} *)

val suite : test
(** [suite] is the test suite for abstract syntax trees. *)

(** {2 Assertions} *)

val assert_ast_equal : ctxt:test_ctxt -> Ast.t -> Ast.t -> unit
(** [assert_ast_equal expected actual] asserts that the abstract syntax tree
    [actual] is equal to the abstract syntax tree [expected].  This is syntactic
    equality, not semantic equality.  I.e., [1 + 2] equals [1 + 2], but not [3].
    *)
