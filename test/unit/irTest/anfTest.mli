open OUnit2
(* open Ir *)

(** {1 Administrative Normal Form Intermediate Representation} *)

val suite : test
(** [suite] is the test suite for the Administrative Normal Form intermediate
    representation. *)

(** {2 Assertions} *)
(*
val assert_un_equal : ctxt:test_ctxt -> Anf.un -> Anf.un -> unit
(** [assert_un_equal ~ctxt expected actual] asserts that the unary operator
    [actual] is equal to the unary operator [expected]. *)

val assert_bin_equal : ctxt:test_ctxt -> Anf.bin -> Anf.bin -> unit
(** [assert_bin_equal ~ctxt expected actual] asserts that the binary operator
    [actual] is equal to the binary operator [expected]. *)

val assert_prim_equal : ctxt:test_ctxt -> Anf.prim -> Anf.prim -> unit
(** [assert_prim_equal ~ctxt expected actual] asserts that the primitive
    [actual] is equal to the primitive [expected].  This is syntactic equality,
    not semantic equality.  I.e., [1 + 2] equals [1 + 2], but not [3]. *)

val assert_stmt_equal : ctxt:test_ctxt -> Anf.stmt -> Anf.stmt -> unit
(** [assert_stmt_equal ~ctxt expected actual] asserts that the statement
    [actual] is equal to the statement [expected].  This is syntactic equality,
    not semantic equality.  I.e., [1 + 2] equals [1 + 2], but not [3]. *) *)
