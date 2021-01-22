open OUnit2
open Nile.Ir

(** {1 Administrative Normal Form Intermediate Representation} *)

val suite : test
(** [suite] is the test suite for the ANF IR. *)

(** {2 Assertions} *)

val assert_atom_equal : ctxt:test_ctxt -> Anf.atom -> Anf.atom -> unit
(** [assert_atom_equal ~ctxt expected actual] asserts that the ANF atom [actual]
    is equal to the ANF atom [expected]. *)

val assert_expr_equal : ctxt:test_ctxt -> Anf.expr -> Anf.expr -> unit
(** [assert_expr_equal ~ctxt expected actual] asserts that the ANF expression
    [actual] is equal to the ANF expression [expected]. *)

val assert_block_equal : ctxt:test_ctxt -> Anf.block -> Anf.block -> unit
(** [assert_block_equal ~ctxt expected actual] asserts that the ANF block
    [actual] is equal to the ANF block [expected]. *)

val assert_top_equal : ctxt:test_ctxt -> Anf.top -> Anf.top -> unit
(** [assert_block_equal ~ctxt expected actual] asserts that the ANF top-level
    statement [actual] is equal to the ANF top-level statement [expected]. *)

val assert_file_equal : ctxt:test_ctxt -> Anf.file -> Anf.file -> unit
(** [assert_block_equal ~ctxt expected actual] asserts that the ANF top-level
    statement [actual] is equal to the ANF top-level statement [expected]. *)
