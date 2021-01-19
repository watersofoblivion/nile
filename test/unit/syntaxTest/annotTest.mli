open OUnit2
open Nile.Syntax

(** {1 Abstract Syntax} *)

val suite : test
(** [suite] is the test suite for abstract syntax trees. *)

(** {2 Assertions}

Assert on the equality of annotated abstract syntax trees.  These assertions
compare syntactic equality, not semantic equality.  I.e., [1 + 2] equals
[1 + 2], but not [3].
*)

val assert_expr_equal : ctxt:test_ctxt -> Annot.expr -> Annot.expr -> unit
(** [assert_expr_equal ~ctxt expected actual] asserts that the expression
    [actual] is equal to the expression [expected]. *)

val assert_top_equal : ctxt:test_ctxt -> Annot.top -> Annot.top -> unit
(** [assert_top_equal ~ctxt expected actual] asserts that the top-level binding
    [actual] is equal to the top-level binding [expected]. *)

val assert_file_equal : ctxt:test_ctxt -> Annot.file -> Annot.file -> unit
(** [assert_file_equal ~ctxt expected actual] asserts that the source file
    [actual] is equal to the source file [expected]. *)
