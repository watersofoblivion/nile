open Lexing

(** {1 Lexical Analyzer} *)

(** {2 Constructors} *)

val from_string : string -> lexbuf
(** [from_string str] initialize a lexing buffer from [str].  Location tracking
    is initialized with the filename [-]. *)

val from_file : string -> lexbuf
(** [from_string path] initialize a lexing buffer from the file located at
    [path].  Location tracking is initialized with the filename [path]. *)

val from_channel : ?fname:string -> in_channel -> lexbuf
(** [from_channel ~fname ic] initialize a lexing buffer from the input channel
    [ic].  Location tracking is initialized with the filename [fname], which
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

(** {3 Keywords} *)

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

(** {3 Operators} *)

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

val op_and : lexbuf -> Parser.token
(** [op_and lexbuf] constructs a token for the [&&] operator. *)

val op_or : lexbuf -> Parser.token
(** [op_or lexbuf] constructs a token for the [||] operator. *)

val op_not : lexbuf -> Parser.token
(** [op_not lexbuf] constructs a token for the [!] operator. *)

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

(** {3 Literals} *)

val lit_true : lexbuf -> Parser.token
(** [lit_true lexbuf] constructs a token for a literal [true] value. *)

val lit_false : lexbuf -> Parser.token
(** [lit_false lexbuf] constructs a token for a literal [false] value. *)

val lit_int : lexbuf -> Parser.token
(** [lit_int lexbuf] constructs a token for a literal int value. *)

(** {3 Identifiers} *)

val uident : lexbuf -> Parser.token
(** [uident lexbuf] constructs a token for an identifier starting with an
    upper-case character. *)

val lident : lexbuf -> Parser.token
(** [lident lexbuf] constructs a token for an identifier starting with a
    lower-case character. *)
