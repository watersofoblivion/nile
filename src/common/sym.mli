(** {1 Symbolization} *)

(** {2 Symbols} *)

type sym
(** A symbol *)

type 'a map

val empty : 'a map
val bind : sym -> 'a -> 'a map -> 'a map
val lookup : sym -> 'a map -> 'a

(** {2 Name-to-Symbol Tables} *)

type tbl
(** A symbol table *)

val tbl : tbl
(** [tbl] returns an empty symbol table *)

val symbolize : string -> tbl -> (sym * tbl)
(** [symbolize str tbl] converts [str] into a symbol from the table [tbl].  If
    [str] has already been symbolized, the same symbol and an unaltered table
    are returned.  Otherwise, a new symbol is generated and returned along with
    a copy of the table with the symbol bound. *)

(** {2 Symbol-to-Name Mappings} *)

type names
(** A mapping from symbols to names *)

val names : tbl -> names
(** [names tbl] constructs a mapping from symbols to names by reversing the
    table [tbl]. *)

val name_of : sym -> names -> string
(** [name_of sym tbl] looks up the string representation of the symbol [sym] in
    the table [tbl].  Raises {!Not_found} if [sym] does not exist in [tbl]. *)
