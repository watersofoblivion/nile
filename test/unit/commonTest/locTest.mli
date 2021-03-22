open OUnit2
open Nile.Syntax

(** {1 Location Tracking} *)

val suite : test
(** [suite] is the test suite for location tracking. *)

(** {2 Values} *)

val dummy : Loc.t
(** [dummy] is a location guaranteed to be different from any location
    generated by a lexer. *)

val gen : unit -> Loc.t
(** [gen ()] generates a location that is guaranteed to be unique from all other
    locations generated by [gen]. *)

(** {2 Assertions} *)

val assert_loc : ctxt:test_ctxt -> string -> (int * int * int) -> (int * int * int) -> int -> Loc.t -> unit
(** [assert_loc ~ctxt fname (start_line, start_col, start_off) (end_line, end_col, end_off) length actual]
    asserts that the location [actual] has filename [fname], start line, column,
    and offset [start_line], [start_col], and [start_off] (respectively), end
    start line, column, and offset [end_line], [end_col], and [end_off]
    (respectively), and length [len]. *)

val assert_loc_equal : ctxt:test_ctxt -> Loc.t -> Loc.t -> unit
(** [assert_loc_equal ~ctxt expected actual] that the location [actual] is equal
    to the location [expected]. *)