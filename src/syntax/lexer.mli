open Lexing

(** {1 Lexical Analyzer} *)

(** {2 Constructors} *)

val from_string : Loc.src -> string -> lexbuf
(** [from_string str] initialize a lexing buffer from [str].  Location tracking
    is initialized with the source [src]. *)

val from_file : Loc.src -> string -> lexbuf
(** [from_string path] initialize a lexing buffer from the file located at
    [path].  Location tracking is initialized with the source [src], overwritten
    with filename [path]. *)

val from_channel : Loc.src -> in_channel -> lexbuf
(** [from_channel src ic] initialize a lexing buffer from the input channel
    [ic].  Location tracking is initialized with the source [src], which
    defaults to "-". *)

(** {2 Entry Point} *)

val lex : lexbuf -> Parser.token
(** [lex lexbuf] lexes the next token from [lexbuf]. *)

(** {2 Tokens} *)

(** {3 Non-Printable Tokens} *)

val eof : lexbuf -> Parser.token
(** [eof lexbuf] constructs an end-of-file token *)

val newline : (lexbuf -> Parser.token) -> lexbuf -> Parser.token
(** [newline lex lexbuf] processes a newline in the input stream and continues
    lexing with [lex]. *)

(** {3 Punctuation} *)

val punc_lparen : lexbuf -> Parser.token
(** [punc_lparen lexbuf] constructs a token for the [(] punctuation mark. *)

val punc_rparen : lexbuf -> Parser.token
(** [punc_rparen lexbuf] constructs a token for the [)] punctuation mark. *)

val punc_lbracket : lexbuf -> Parser.token
(** [puct_lbracket lexbuf] constructs a token for the [[] punctuation mark. *)

val punc_rbracket : lexbuf -> Parser.token
(** [puct_rbracket lexbuf] constructs a token for the [\]] punctuation mark. *)

val punc_lbrace : lexbuf -> Parser.token
(** [puct_lbrace lexbuf] constructs a token for the [{] punctuation mark. *)

val punc_rbrace : lexbuf -> Parser.token
(** [puct_rbrace lexbuf] constructs a token for the [}] punctuation mark. *)

val punc_colon : lexbuf -> Parser.token
(** [punc_colon lexbuf] constructs a token for the [:] punctuation mark. *)

val punc_arrow : lexbuf -> Parser.token
(** [punc_arrow lexbuf] constructs a token for the [->] punctuation mark. *)

val punc_darrow : lexbuf -> Parser.token
(** [punc_darrow lexbuf] constructs a token for the [=>] punctuation mark. *)

val punc_bind : lexbuf -> Parser.token
(** [punc_bind lexbuf] constructs a token for the [=] punctuation mark. *)

val punc_comma : lexbuf -> Parser.token
(** [punc_comma lexbuf] constructs a token for the [,] punctuation mark. *)

val punc_ground : lexbuf -> Parser.token
(** [punc_ground lexbuf] constructs a token for the [_] punctuation mark. *)

val punc_pipe : lexbuf -> Parser.token
(** [punc_pipe lexbuf] constructs a token for the [|] punctuation mark. *)

val punc_elipsis : lexbuf -> Parser.token
(** [punc_elipsis lexbuf] constructs a token for the [...] punctuation mark. *)

(** {3 Keywords} *)

val kwd_type : lexbuf -> Parser.token
(** [kwd_type lexbuf] constructs a token for the [type] keyword. *)

val kwd_val : lexbuf -> Parser.token
(** [kwd_val lexbuf] constructs a token for the [val] keyword. *)

val kwd_def : lexbuf -> Parser.token
(** [kwd_def lexbuf] constructs a token for the [def] keyword. *)

val kwd_let : lexbuf -> Parser.token
(** [kwd_let lexbuf] constructs a token for the [let] keyword. *)

val kwd_rec : lexbuf -> Parser.token
(** [kwd_rec lexbuf] constructs a token for the [rec] keyword. *)

val kwd_and : lexbuf -> Parser.token
(** [kwd_and lexbuf] constructs a token for the [and] keyword. *)

val kwd_in : lexbuf -> Parser.token
(** [kwd_in lexbuf] constructs a token for the [in] keyword. *)

val kwd_if : lexbuf -> Parser.token
(** [kwd_if lexbuf] constructs a token for the [if] keyword. *)

val kwd_then : lexbuf -> Parser.token
(** [kwd_then lexbuf] constructs a token for the [then] keyword. *)

val kwd_else : lexbuf -> Parser.token
(** [kwd_else lexbuf] constructs a token for the [else] keyword. *)

val kwd_case : lexbuf -> Parser.token
(** [kwd_case lexbuf] constructs a token for the [case] keyword. *)

val kwd_of : lexbuf -> Parser.token
(** [kwd_of lexbuf] constructs a token for the [of] keyword. *)

val kwd_end : lexbuf -> Parser.token
(** [kwd_end lexbuf] constructs a token for the [end] keyword. *)

val kwd_from : lexbuf -> Parser.token
(** [kwd_from lexbuf] constructs a token for the [from] keyword. *)

val kwd_import : lexbuf -> Parser.token
(** [kwd_import lexbuf] constructs a token for the [import] keyword. *)

val kwd_package : lexbuf -> Parser.token
(** [kwd_package lexbuf] constructs a token for the [package] keyword. *)

val kwd_as : lexbuf -> Parser.token
(** [kwd_as lexbuf] constructs a token for the [as] keyword. *)

(** {3 Operators} *)

val op_band : lexbuf -> Parser.token
(** [op_band lexbuf] constructs a token for the [&] operator. *)

val op_bxor : lexbuf -> Parser.token
(** [op_bxor lexbuf] constructs a token for the [^] operator. *)

val op_bnot : lexbuf -> Parser.token
(** [op_bnot lexbuf] constructs a token for the [~] operator. *)

val op_lsl : lexbuf -> Parser.token
(** [op_lsl lexbuf] constructs a token for the [<<] operator. *)

val op_lsr : lexbuf -> Parser.token
(** [op_lsr lexbuf] constructs a token for the [>>] operator. *)

val op_asl : lexbuf -> Parser.token
(** [op_asl lexbuf] constructs a token for the [<<<] operator. *)

val op_asr : lexbuf -> Parser.token
(** [op_asr lexbuf] constructs a token for the [>>>] operator. *)

val op_add : lexbuf -> Parser.token
(** [op_add lexbuf] constructs a token for the [+] operator. *)

val op_sub : lexbuf -> Parser.token
(** [op_sub lexbuf] constructs a token for the [-] operator. *)

val op_mul : lexbuf -> Parser.token
(** [op_mul lexbuf] constructs a token for the [*] operator. *)

val op_div : lexbuf -> Parser.token
(** [op_div lexbuf] constructs a token for the [/] operator. *)

val op_mod : lexbuf -> Parser.token
(** [op_mod lexbuf] constructs a token for the [%] operator. *)

val op_land : lexbuf -> Parser.token
(** [op_land lexbuf] constructs a token for the [&&] operator. *)

val op_lor : lexbuf -> Parser.token
(** [op_lor lexbuf] constructs a token for the [||] operator. *)

val op_lnot : lexbuf -> Parser.token
(** [op_lnot lexbuf] constructs a token for the [!] operator. *)

val op_eq : lexbuf -> Parser.token
(** [op_eq lexbuf] constructs a token for the [==] operator. *)

val op_neq : lexbuf -> Parser.token
(** [op_neq lexbuf] constructs a token for the [!=] operator. *)

val op_lte : lexbuf -> Parser.token
(** [op_lte lexbuf] constructs a token for the [<=] operator. *)

val op_lt : lexbuf -> Parser.token
(** [op_lt lexbuf] constructs a token for the [<] operator. *)

val op_gt : lexbuf -> Parser.token
(** [op_gt lexbuf] constructs a token for the [>] operator. *)

val op_gte : lexbuf -> Parser.token
(** [op_gte lexbuf] constructs a token for the [>=] operator. *)

val op_dot : lexbuf -> Parser.token
(** [op_dot lexbuf] constructs a token for the [.] operator. *)

val op_cons : lexbuf -> Parser.token
(** [op_cons lexbuf] constructs a token for the [::] operator. *)

(** {3 Literals} *)

val lit_unit : lexbuf -> Parser.token
(** [lit_unit lexbuf] constructs a token for a literal unit value. *)

val lit_bool : lexbuf -> Parser.token
(** [lit_bool lexbuf] constructs a token for a literal boolean value. *)

val lit_int : int -> lexbuf -> Parser.token
(** [lit_int radix lexbuf] constructs a token for a literal int value with radix
    [radix]. *)

val lit_float : bool -> lexbuf -> Parser.token
(** [lit_float hex lexbuf] constructs a token for a literal floating-point
    value.  If [hex] is true, the value is in hexadecimal format. *)

val lit_rune : lexbuf -> Parser.token
(** [lit_rune lexbuf] constructs a token for a literal rune value. *)

val lit_string : lexbuf -> Parser.token
(** [lit_string lexbuf] constructs a token for a literal string value. *)

val lit_byte : lexbuf -> Parser.token
(** [lit_byte lexbuf] constructs a token for a literal byte value. *)

val lit_blob : lexbuf -> Parser.token
(** [lit_blob lexbuf] constructs a token for a literal binary large object
    (BLOB) value. *)

val lit_timestamp : lexbuf -> Parser.token
(** [lit_timestamp lexbuf] constructs a token for a literal ISO8601 timestamp
    value. *)

val lit_duration : lexbuf -> Parser.token
(** [lit_duration lexbuf] constructs a token for a literal ISO8601 duration
    value. *)

(** {3 Identifiers} *)

val uident : lexbuf -> Parser.token
(** [uident lexbuf] constructs a token for an identifier starting with an
    upper-case character. *)

val lident : lexbuf -> Parser.token
(** [lident lexbuf] constructs a token for an identifier starting with a
    lower-case character. *)
