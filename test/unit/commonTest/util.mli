open Format
open OUnit2

(** {1 Assertions} *)

val never : 'a -> 'a -> bool
(** [never _ _] is a predicate usable for the [~cmp] argument of
    {!OUnit2.assert_equals} that constantly returns [false]. *)

val printer : ('a -> formatter -> unit) -> 'a -> string
(** [printer pp x] constructs a printer usable for the [~printer] argument of
    {!OUnit2.assert_equals} from the pretty-printing function [pp]. *)

val assert_pp : ('a -> formatter -> unit) -> ctxt:test_ctxt -> string list -> 'a -> unit
(** [assert_pp pp ~ctxt lines expr] asserts that pretty-printing [expr] using
    the pretty-printing function [pp] produces the same output as concatenating
    [lines] together with newline (['\n']) characters. *)
