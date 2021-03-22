open Common

(** {1 Abstract Syntax}
 *
 * The external syntax of the language.  This includes not only the parsed
 * values, but in some cases the lexeme parsed.  This is to be able to
 * accurately pretty-print the source code without it having to be fully valid.
 * This is useful for both formatting and for error messages.
 *)

(** {2 Syntax}
 *
 * Data types representing all of the various constructs in the external syntax.
 *)

(** {3 Numeric Options}
 *
 * Properties of numbers relevant to parsing and printing.
 *)

type radix = private
  | Binary  (** Binary *)
  | Octal   (** Octal *)
  | Decimal (** Decimal *)
  | Hex     (** Hexadecimal *)
(** Numeric Radix *)

(** {3 Format Strings Options}
 *
 * Options that can be given as part of format string interpolations.
 *)

type wp = private
  | Natural       (** Unspecified *)
  | Static of {
      min: int; (** Minimum *)
    } (** Given at compile time *)
  | Dynamic       (** Given at runtime (as an argument) *)
(** The width or precision of a format string interpolation *)

type justification = private
  | Left  (** Left Justified *)
  | Right (** Right Justified *)
(** Format string interpolation justification *)

type signedness = private
  | Unsigned (** Unsigned *)
  | Signed   (** Signed, only print negative signs *)
  | Positive (** Signed, always print sign *)
  | Space    (** Signed, print space for positive sign. *)
(** Format string integer interpolation sign handling *)

(** {3 Format BLOB (Binary Large Object) Options}
 *
 * Options that can be given as part of format BLOB (Binary Large Object)
 * interpolations.
 *)

type endianness = private
  | Big    (** Big endian *)
  | Little (** Little endian *)
(** Endianness *)

(** {3 Expressions}
 *
 * Core expressions in the language.
 *)

type expr = private
  | Unit of {
      loc: Loc.t; (** Location Tracking Information *)
    } (** Unit *)
  | Bool of {
      loc: Loc.t; (** Location Tracking Information *)
      b:   bool   (** Value *)
    } (** Boolean *)
  | Int of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      radix:  radix;  (** Radix *)
      i:      Z.t     (** Value *)
    } (** Integer *)
  | Float of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      hex:    bool;   (** Is Hex Notation *)
      q:      Q.t     (** Value *)
    } (** Floating-point *)
  | Rune of {
      loc:  Loc.t;   (** Location Tracking Information *)
      r:    Uchar.t; (** Value *)
      size: int      (** Size in bytes *)
    } (** Rune *)
  | String of {
      loc: Loc.t;   (** Location Tracking Information *)
      fmt: fmt list (** Format Components *)
    } (** String *)
  | Byte of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      radix:  radix;  (** Radix *)
      b:      bytes   (** Value *)
    } (** Byte *)
  | Blob of {
      loc:    Loc.t;             (** Location Tracking Information *)
      endian: endianness option; (** Endianness *)
      fmt:    blb_fmt list       (** Format Components *)
    } (** Binary Large Object (BLOB) *)
  | Timestamp of {
      loc:        Loc.t;       (** Location Tracking Information *)
      extended:   bool;        (** Extended Format *)
      year:       expr option; (** Year *)
      month:      expr option; (** Month *)
      week:       expr option; (** Week of year *)
      dow:        expr option; (** Day of week *)
      day:        expr option; (** Day of month *)
      ordinal:    expr option; (** Ordinal day *)
      hour:       expr option; (** Hour *)
      minue:      expr option; (** Minute *)
      second:     expr option; (** Second *)
      fraction:   expr option; (** Fractional value *)
      utc:        bool;        (** UTC Timezone *)
      off_sign:   bool;        (** Offset Sign *)
      off_hour:   expr option; (** Offset hour *)
      off_minute: expr option; (** Offset minutes *)
    } (** ISO-8601 Timestamp *)
  | Duration of {
      loc:      Loc.t;       (** Location Tracking Information *)
      extended: bool;        (** Extended Format *)
      years:    expr option; (** Years *)
      months:   expr option; (** Months *)
      weeks:    expr option; (** Weeks *)
      days:     expr option; (** Days *)
      hours:    expr option; (** Hours *)
      minutes:  expr option; (** Minutes *)
      seconds:  expr option; (** Seconds *)
      fraction: expr option; (** Fractional value *)
    } (** ISO-8601 Duration *)
  | Tuple of {
      loc:   Loc.t;    (** Location Tracking Information *)
      exprs: expr list (** Element Values *)
    } (** Tuples *)
  | Record of {
      loc:    Loc.t;          (** Location Tracking Information *)
      constr: Sym.sym option; (** Type Constructor *)
      fields: field list      (** Field Constructors *)
    } (** Record *)
  | Var of {
      loc: Loc.t;  (** Location Tracking Information *)
      id:  Sym.sym (** Name *)
    } (** Variable Identifier *)
  | UnOp of {
      loc:  Loc.t; (** Location Tracking Information *)
      op:   Op.un; (** Operator *)
      prec: int;   (** Precedence *)
      rhs:  expr   (** Right-Hand Side *)
    } (** Unary Operation *)
  | BinOp of {
      loc:  Loc.t;  (** Location Tracking Information *)
      op:   Op.bin; (** Operator *)
      prec: int;    (** Precedence *)
      lhs:  expr;   (** Left-Hand Side *)
      rhs:  expr    (** Right-Hand Side *)
    } (** Binary Operation *)
  | Slice of {
      loc:   Loc.t;       (** Location Tracking Information *)
      expr:  expr;        (** Value to Slice *)
      start: expr option; (** Beginning *)
      stop:  expr option  (** End *)
    } (** Slice *)
  | Index of {
      loc:  Loc.t; (** Location Tracking Information *)
      expr: expr;  (** Value to Index *)
      idx:  expr   (** Index *)
    } (** Index *)
  | If of {
      loc:  Loc.t; (** Location Tracking Information *)
      cond: expr;  (** Condition *)
      tru:  expr;  (** True Branch *)
      fls:  expr   (** False Branch *)
    } (** Conditional *)
  | Case of {
      loc:     Loc.t;      (** Location Tracking Information *)
      scrut:   expr;       (** Scurtinee *)
      clauses: clause list (** Clauses *)
    } (** Case *)
  | Bind of {
      loc:     Loc.t;   (** Location Tracking Information *)
      binding: binding; (** Binding *)
      scope:   expr     (** Scope of Binding *)
    } (** Value Binding *)
  | BindRec of {
      loc:      Loc.t;        (** Location Tracking Information *)
      bindings: binding list; (** Bindings *)
      scope:    expr          (** Scope of Bindings *)
    } (** Recursive Value Bindings *)
  | Abs of {
      loc:    Loc.t;         (** Location Tracking Information *)
      params: param list;    (** Parameters *)
      res:    Type.t option; (** Result Type *)
      body:   expr           (** Body *)
    } (** Function Abstraction *)
  | App of {
      loc:  Loc.t;    (** Location Tracking Information *)
      fn:   expr;     (** Function *)
      args: expr list (** Arguments *)
    } (** Function Application *)
(** An expression *)

and field = Field of {
  loc:   Loc.t;   (** Location Tracking Information *)
  name:  Sym.sym; (** Field Name *)
  value: expr     (** Field Value *)
}
(** A record field *)

and param = Param of {
  loc:  Loc.t;  (** Location Tracking Information *)
  name: Patt.t; (** Bound Name *)
  ty:   Type.t  (** Type *)
}
(** A parameter *)

and binding = Binding of {
  loc:  Loc.t;         (** Location Tracking Information *)
  name: Patt.t;        (** Bound Name *)
  ty:   Type.t option; (** Type *)
  body: expr           (** Bound Value *)
}
(** A value Binding *)

and clause = Clause of {
  loc:  Loc.t;  (** Location Tracking Information *)
  patt: Patt.t; (** Pattern to Match *)
  body: expr    (** Body *)
}
(** A pattern matching clause *)

and fmt = private
  | FmtConst of {
      loc:  Loc.t;        (** Location Tracking Information *)
      str:  Uchar.t list; (** Constant Value *)
      len:  int;          (** Length in runes *)
      size: int           (** Size in bytes *)
    } (** Constant Value *)
  | FmtPct of {
      loc:   Loc.t;     (** Location Tracking Information *)
      width: int option (** Minimum Width *)
    } (** Percent Interpolation *)
  | FmtBool of {
      loc:   Loc.t;      (** Location Tracking Information *)
      width: int option; (** Minimum Width *)
      expr:  expr option (** Value Expression *)
    } (** Boolean Interpolation *)
  | FmtInt of {
      loc:           Loc.t;         (** Location Tracking Information *)
      width:         wp;            (** Minimum Width *)
      discrim:       Uchar.t;       (** Discriminator Value *)
      radix:         radix;         (** Radix *)
      expr:          expr option;   (** Value Expression *)
      sign:          signedness;    (** Signedness *)
      justification: justification; (** Justification *)
      capital:       bool;          (** Use capitals for hexadecimal values *)
      alt:           bool;          (** Use alternate formatting.  Underscores for decimal numbers, or [0<radix>] for non-decimal numbers. *)
    } (** Integer Interpolation *)
  | FmtFloat of {
      loc:           Loc.t;         (** Location Tracking Information *)
      width:         wp;            (** Minimum Width *)
      precision:     wp;            (** Precision *)
      expr:          expr option;   (** Value Expression *)
      sign:          signedness;    (** Signedness *)
      justification: justification; (** Justification *)
    } (** Floating Point Interpolation *)
  | FmtRune of {
      loc:           Loc.t;        (** Location Tracking Information *)
      width:         wp;           (** Minimum Width *)
      discrim:       Uchar.t;      (** Discriminator Value *)
      expr:          expr option;  (** Value Expression *)
      quote:         bool;         (** Quoted *)
      justification: justification (** Justification *)
    } (** Rune Interpolation *)
  | FmtString of {
      loc:           Loc.t;        (** Location Tracking Information *)
      width:         wp;           (** Minimum Width *)
      expr:          expr option;  (** Value expression *)
      quote:         bool;         (** Quoted *)
      justification: justification (** Justification *)
    } (** String Interpolation *)
(** Format String Components and Interpolations *)

and blb_fmt = private
  | BlbFmtConst of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      bs:     bytes;  (** Value *)
      size:   int     (** Size *)
    } (** Constant *)
  | BlbFmtBool of {
      loc:    Loc.t;             (** Location Tracking Information *)
      width:  int;               (** Width in bytes, must be a power of 2 *)
      endian: endianness option; (** Endianness *)
      expr:   expr option        (** Value Expression *)
    } (** Boolean Interpolation *)
  | BlbFmtInt of {
      loc:    Loc.t;             (** Location Tracking Information *)
      width:  int;               (** Width in bytes, must be a power of 2 *)
      endian: endianness option; (** Endianness *)
      expr:   expr option        (** Value Expression *)
    } (** Integer Interpolation *)
  | BlbFmtFloat of {
      loc: Loc.t;       (** Location Tracking Information *)
      expr: expr option (** Value Expression *)
    } (** Floating-point Interpolation *)
  | BlbFmtRune of {
      loc: Loc.t;                (** Location Tracking Information *)
      endian: endianness option; (** Endianness *)
      expr: expr option          (** Value Expression *)
    } (** Rune Interpolation *)
  | BlbFmtString of {
      loc: Loc.t;                (** Location Tracking Information *)
      endian: endianness option; (** Endianness *)
      expr: expr option          (** Value Expression *)
    } (** String Interpolation *)
  | BlbFmtByte of {
      loc: Loc.t;       (** Location Tracking Information *)
      expr: expr option (** Value Expression *)
    } (** Byte Interpolation *)
  | BlbFmtBlob of {
      loc: Loc.t;       (** Location Tracking Information *)
      expr: expr option (** Value Expression *)
    } (** BLOB Interpolation *)
(** Format BLOB Components and Interpolations *)

and ts_fmt = private
  | TsFmtConst of {
      loc: Loc.t;       (** Location Tracking Information *)
      str: Uchar.t list (** Value *)
    } (** Constant *)
  | TsFmtPct of {
      loc:   Loc.t; (** Location Tracking Information *)
      width: wp     (** Minimum Width *)
    } (** Percent Interpolation *)
  | TsFmtWeekDay of {
      loc:    Loc.t;       (** Location Tracking Information *)
      width:  wp;          (** Minimum Width *)
      expr:   expr option; (** Value Expression *)
      named:  bool;        (** Use the name instead of the number *)
      monday: bool;        (** Monday is the first day of the week *)
      full:   bool         (** Use the full weekday name  *)
    } (** Week Day Interpolation *)
  | TsFmtMonthDay of {
      loc:   Loc.t;       (** Location Tracking Information *)
      width: wp;          (** Minimum Width *)
      expr:  expr option; (** Value Expression *)
      pad:   bool;        (** Pad the day to two digits with leading zeros *)
    } (** Month Day Interpolation *)
  | TsFmtMonth of {
      loc:   Loc.t;       (** Location Tracking Information *)
      width: wp;          (** Minimum Width *)
      expr:  expr option; (** Value Expression *)
      named: bool;        (** Use the month name instead of the number *)
      full:  bool         (** Use the full month name or pad the month number to two digits with leading zeros. *)
    } (** Month Interpolation *)
  | TsFmtYear of {
      loc:     Loc.t;       (** Location Tracking Information *)
      width:   wp;          (** Minimum Width *)
      expr:    expr option; (** Value Expression *)
      century: bool;        (** Display Century *)
      pad:     bool         (** Pad the year to two or four digits with leading zeros *)
    } (** Year Interpolation *)
  | TsFmtHour of {
      loc:         Loc.t;       (** Location Tracking Information *)
      width:       wp;          (** Minimum Width *)
      expr:        expr option; (** Value Expression *)
      twenty_four: bool;        (** Use a 24-hour clock *)
      pad:         bool;        (** Pad the hour to two digits with leading zeros *)
    } (** Month Interpolation *)
  | TsFmtAmPm of {
      loc:     Loc.t;       (** Location Tracking Information *)
      width:   wp;          (** Minimum Width *)
      expr:    expr option; (** Value Expression *)
      capital: bool         (** Use capital letters *)
    } (** AM/PM Interpolation *)
  | TsFmtMinute of {
      loc:   Loc.t;       (** Location Tracking Information *)
      width: wp;          (** Minimum Width *)
      expr:  expr option; (** Value Expression *)
      pad:   bool         (** Pad to two digits with leading zeros *)
    } (** Minutes Interpolation *)
  | TsFmtSecond of {
      loc:   Loc.t;       (** Location Tracking Information *)
      width: wp;          (** Minimum Width *)
      expr:  expr option; (** Value Expression *)
      pad:   bool         (** Pad to two digits with leading zeros *)
    } (** Seconds Interpolation *)
  | TsFmtSubsec of {
      loc:       Loc.t;      (** Location Tracking Information *)
      width:     wp;         (** Minimum Width *)
      precision: wp;         (** Number of digits to display *)
      expr:      expr option (** Value Expression *)
    } (** Subsecond Interpolation *)
  | TsFmtZone of {
      loc: Loc.t
    } (** Zone Expression *)
  | TsFmtYearDay of {
      loc:   Loc.t;       (** Location Tracking Information *)
      width: wp;          (** Minimum Width *)
      expr:  expr option; (** Value Expression *)
      pad:   bool         (** Pad to three digits with leading zeros *)
    } (** Day of Year Interpolation *)
  | TsFmtYearWeek of {
      loc:   Loc.t;       (** Location Tracking Information *)
      width: wp;          (** Minimum Width *)
      expr:  expr option; (** Value Expression *)
      pad:   bool         (** Pad to two digits with leading zeros *)
    } (** Week of Year Interpolation *)
(** Format Timestamp Components and Interpolations *)

(** {3 Top-Level Statements}
 *
 * Statements that occur at the top-level of an individual file, excluding the
 * file-structure statements (such as [package] and [import] declarations.
 *)

type top = private
  | Val of {
      loc:     Loc.t;  (** Location Tracking Information *)
      binding: binding (** Binding *)
    } (** Value binding *)
  | Def of {
      loc:     Loc.t;  (** Location Tracking Information *)
      binding: binding (** Bindings *)
    } (** Function binding *)
  | Type of {
      loc:  Loc.t;   (** Location Tracking Information *)
      name: Sym.sym; (** Name *)
      defn: Type.t   (** Definition *)
    } (** Type binding *)
(** A top-level statement *)

(** {3 Import Statements}
 *
 * Dependencies, imports, and local names.
 *)

type name = Name of {
  loc:  Loc.t;  (** Location Tracking Information *)
  name: Sym.sym (** Package Name *)
}
(** Import name *)

type version = Version of {
  loc:   Loc.t; (** Location Tracking Information *)
  major: int    (** Major Version Number *)
}
(** Major version number *)

type src = Source of {
  loc:     Loc.t;  (** Location Tracking Information *)
  mojule:  name;   (** Module Name *)
  version: version (** Major Version *)
}

type from = From of {
  loc:    Loc.t;     (** Location Tracking Information *)
  source: src option (** Source Module *)
}
(** From clause.  If the name/version pair is given, it specifies an external
    package to import.  Otherwise, it specifies the current package. *)

type alias = Alias of {
  loc:     Loc.t;      (** Location Tracking Information *)
  package: name;       (** Package Path *)
  alias:   name option (** Local Package Alias *)
}
(** Package name and optional local alias.  If no local alias is given, the
    module's declared name is used. *)

type pkgs = Packages of {
  loc:     Loc.t;     (** Location Tracking Information *)
  clauses: alias list (** Package Clauses *)
}
(** List of packages to import from a module. *)

type import = Import of {
  loc:      Loc.t;       (** Location Tracking Information *)
  from:     from option; (** From Clause *)
  packages: pkgs         (** Package and Alias List *)
}
(** Import statement *)

(** {3 Package Statements}
 *
 * The opening [package] statement of a file.
 *)

type pkg = Package of {
  loc:  Loc.t; (** Location Tracking Information *)
  name: name   (** Package Name *)
}
(** Package statement declaring the name of the package this file is in. *)

(** {3 Source Files}
 *
 * A source file of the language.  This combines all of the previously defined
 * components.
 *)

type file = File of {
  package: pkg;         (** Package Name *)
  imports: import list; (** Imported Packages *)
  tops:    top list     (** Top-Level Statements *)
}
(** A source file *)

(** {2 Constructors}
 *
 * Functions to construct Abstract Syntax Trees.
 *)

(** {3 Numeric Options} *)

val binary : radix
(** [binary] constructs a binary radix. *)

val octal : radix
(** [octal] constructs an octal radix. *)

val decimal : radix
(** [decimal] constructs a decimal radix. *)

val hex : radix
(** [hex] constructs a hexadecimal radix. *)

(** {3 Format String Options} *)

(** {4 Width and Precision} *)

val natural : wp
(** [natural] constructs the natural width for a format string interpolation. *)

val static : int -> wp
(** [static min] constructs a statically defined minimum width of [min] for a
   format string interpolation. *)

val dynamic : wp
(** [dynamic] constructs a dynamically defined minimum width for a format string
   interpolation. *)

(** {4 Justification} *)

val left : justification
(** [left] constructs a left justification for a format string interpolation. *)

val right : justification
(** [right] constructs a right justification for a format string interpolation.
   *)

(** {4 Signedness} *)

val unsigned : signedness
(** [unsigned] constructs an unsigned signedness for a format string
   interpolation. *)

val signed : signedness
(** [signed] constructs an signed signedness for a format string
   interpolation. *)

val positive : signedness
(** [positive] constructs an positive signedness for a format string
   interpolation. *)

val space : signedness
(** [space] constructs an space signedness for a format string interpolation. *)

(** {3 Format BLOB (Binary Large Object) Options} *)

val big : endianness
(** [big] constructs a big endianness for a format BLOB interpolation. *)

val little : endianness
(** [little] constructs a little endianness for a format BLOB interpolation. *)

(** {3 Expressions} *)

val unit : Loc.t -> expr
(** [unit loc] constructs a unit literal at location [loc]. *)

val bool : Loc.t -> bool -> expr
(** [bool loc lexeme] constructs a boolean literal with the lexeme [lexeme] at
    location [loc]. *)

val int : Loc.t -> string -> radix -> Z.t -> expr
(** [int loc lexeme radix i] constructs an integer literal with the lexeme
    [lexeme], radix [radix], and value [i] at location [loc].  The lexeme should
    include the [0<radix>] prefix, if present. *)

val float : Loc.t -> string -> bool -> Q.t -> expr
(** [float loc lexeme hex q] constructs a floating-point literal with the lexeme
    [lexeme] and the rational value [q] at location [loc].  If [hex] is true,
    then the lexeme is in hexadecimal form.  The lexeme should include the [0f]
    prefix, if present. *)

val rune : Loc.t -> Uchar.t -> expr
(** [rune loc r] constructs a rune literal with the value [r] at location [loc].
    The location should include the opening and closing single quotes. *)

val string : Loc.t -> fmt list -> expr
(** [string loc fmt] constructs a string literal with the format components
    [fmt] at location [loc].  The location should include the opening and
    closing double quotes. *)

val byte : Loc.t -> string -> radix -> bytes -> expr
(** [byte loc lexeme radix b] constructs a byte literal with the lexeme
    [lexeme], radix [radix], and value [b] at location [loc].  The lexeme should
    include the [\<radix>] prefix. *)

val blob : Loc.t -> endianness -> blb_fmt list -> expr
(** [blob loc endian fmt] constructs a binary large object (BLOB) literal with
    the default endianness [endian] and the format components [fmt] at location
    [loc].  The location should include the back quotes. *)

val timestamp : Loc.t -> bool -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> bool -> bool -> expr option -> expr option -> expr
(** [timestamp loc extended year month week dow day ordinal hour minute second fraction utc off_sign off_hour off_minute]
    constructs a ISO-8601 timestamp literal.  The location should include the
    [@] prefix. The various components are specified with the arguments. *)

val duration : Loc.t -> bool -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr option -> expr
(** [duration loc extended years months weeks days hours minutes seconds fraction]
    constructs a ISO-8601 duration literal with the lexeme [lexeme] at location
    [loc].  The location should include the [@@] prefix.  The various components
    are specified with the arguments. *)

val tuple : Loc.t -> expr list -> expr
(** [tuple loc exprs] constructs a tuple literal with the values [exprs] at
    location [loc].  The location should span from the opening parenthesis to
    the closing parenthesis. *)

val record : Loc.t -> Sym.sym option -> field list -> expr
(** [record loc constr fields] constructs a record literal with the constructor
    [constr] and the fields [fields] at location [loc].  The location should
    span from the beginning of the constructor to the closing brace. *)

val var : Loc.t -> Sym.sym -> expr
(** [var loc sym] constructs a variable identifier expression at location [loc]
    referencing the value bound to the symbol [sym]. *)

val un_op : Loc.t -> Op.un -> int -> expr -> expr
(** [un_op loc op prec r] constructs a unary operator expression at location [loc]
    with the operator [op] of precedence [prec] operating on [r].  The location
    should span from [op] to [r]. *)

val bin_op : Loc.t -> Op.bin -> int -> expr -> expr -> expr
(** [bin_op loc op prec l r] constructs a binary operator expression at location
    [loc] with the operator [op] of precedence [prec] operating on [l] and [r].
    The location should span from [l] to [r]. *)

val slice : Loc.t -> expr -> expr option -> expr option -> expr
(** [slice loc expr start stop] constructs a slice operation at location [loc]
    slicing the value [expr] from [start] to [stop].  The location should span
    from the beginning of [expr] to the final [\]]. *)

val index : Loc.t -> expr -> expr -> expr
(** [index loc expr idx] constructs an index operation at location [loc]
    indexing into the value [expr] at [index].  The location should span
    from the beginning of [expr] to the final [\]]. *)

val cond : Loc.t -> expr -> expr -> expr -> expr
(** [cond loc c t f] constructs a conditional expression at location [loc]
    branching on the value of the expression [c].  If [c] is true, then the
    expression [t] is evaluated.  Otherwise, the expression [f] is evaluated.
    The location should span from the initial "if" keyword to the end of [f]. *)

val case : Loc.t -> expr -> clause list -> expr
(** [case loc scrut clauses] constructs a case expression at location [loc]
    scrutinizing [scrut] and branching to one of [clauses].  The location should
    span from the initial "case" keyword to the end of the last clause. *)

val bind : Loc.t -> binding -> expr -> expr
(** [bind loc b rest] constructs a value binding expression at location [loc]
    binding [b] within the scope of [rest].  The location should span from the
    initial "let" keyword to the end of the [rest] expression. *)

val bind_rec : Loc.t -> binding list -> expr -> expr
(** [bind_rec loc bs rest] constructs a set of recursive value bindings at the
    location [loc] over the list of bindings [bs] within the scope of both each
    other and [rest].  The location should span from the initial "let" keyword
    to the end of the [rest] expression. *)

val abs : Loc.t -> param list -> Type.t option -> expr -> expr
(** [abs loc params res expr] constructs a function abstraction expression at
    location [loc] binding the parameter [params] within the scope of the
    function body [expr] with the result type [res].  The location should span
    from the first element of [params] to the end of [body].  If not provided,
    the type [res] will be inferred. *)

val app : Loc.t -> expr -> expr list -> expr
(** [app loc f xs] constructs a function application expression at location
    [loc] applying the function [f] to the values [xs].  The location should
    span from the beginning of [f] to the end of the last element of [xs]. *)

val field : Loc.t -> Sym.sym -> expr -> field
(** [field loc name expr] constructs a record field at location [loc] binding
    the field [name] to the value [expr].  The location should span from the
    beginning of [name] to the end of [expr]. *)

val param : Loc.t -> Patt.t -> Type.t -> param
(** [param loc patt ty] constructs a function parameter at location [loc]
    binding arguments matching the [patt] of type [ty].  The location should
    span from the beginning of [patt] to the end of [ty]. *)

val binding : Loc.t -> Patt.t -> Type.t option -> expr -> binding
(** [binding loc patt ty expr] constructs a value binding expression at location
    [loc] binding the value [expr] of type [ty] to pattern [patt].  The location
    should span from the beginning of [patt] to the end of [expr].  If not
    provided, the type [ty] will be inferred. *)

val clause : Loc.t -> Patt.t -> expr -> clause
(** [clause loc patt expr] constructs a pattern matching clause at location
    [loc] that executes [expr] when [patt] is matched.  The location should span
    from the beginning of [patt] to the end of [expr]. *)

(** {3 Format Strings} *)

val fmt_const : Loc.t -> Uchar.t list -> fmt
(** [fmt_const loc str] constructs a format string constant component at
    location [loc] with the value [str]. *)

val fmt_pct : Loc.t -> fmt
(** [fmt)_pct loc] constructs a format string percent interpolation at location
    [loc]. *)

val fmt_bool : Loc.t -> expr option -> fmt
(** [fmt_bool loc expr] constructs a format string boolean interpolation at
    location [loc] with the optional value expression [expr]. *)

val fmt_int : Loc.t -> wp -> Uchar.t -> radix -> expr option -> signedness -> justification -> bool -> bool -> fmt
(** [fmt_int loc width discrim radix expr signedness justification capital alt]
    constructs a format string integer interpolation at location [loc] with the
    width [width], the discriminator value [discrim], the radix [radix], and the
    optional value expression [expr].  Sign handling is given by [signedness]
    and justification by [justification].  If [capital] is true, capital letters
    are used when printing hexadecimal.  If [alt] is true, alternate formatting
    is used: for decimal numbers, underscores are added for readability, for
    non-decimal numbers the [0<radix>] prefix is added. *)

val fmt_float : Loc.t -> wp -> wp -> expr option -> signedness -> justification -> fmt
(** [fmt_float loc width precision expr signedness justification] constructs a
    format string floating-point interpolation at location [loc] with width
    [width], precision [precision], and the optional value expression [expr].
    Sign handling is given by [signedness] and justification by [justification].
    *)

val fmt_rune : Loc.t -> wp -> Uchar.t -> expr option -> bool -> justification -> fmt
(** [fmt_rune loc width discrim expr quote justification] constructs a format
    string rune interpolation at location [loc] with width [width],
    discriminator [discrim], and the optional value expression [expr].  If
    [quote] is true, then the rune is quoted.  Justification is given by
    [justification]. *)

val fmt_string : Loc.t -> wp -> expr option -> bool -> justification -> fmt
(** [fmt_string loc width expr quote justification] constructs a format
    string string interpolation at location [loc] with width [width],
    discriminator [discrim], and the optional value expression [expr].  If
    [quote] is true, then the string is quoted.  Justification is given by
    [justification]. *)

(** {3 Format BLOBs (Binary Large Objects)} *)

val blb_fmt_const : Loc.t -> string -> bytes -> blb_fmt
(** [blb_fmt_const loc lexeme bs] constructs a BLOB format constant with lexeme
    [lexeme] and value [bs] at location [loc]. *)

val blb_fmt_bool : Loc.t -> int -> endianness option -> expr option -> blb_fmt
(** [blb_fmt_bool loc width endian expr] constructs a BLOB format boolean
    interpolation with width [width], endianness [endian], and optional value
    expression [expr] at location [loc]. *)

val blb_fmt_int : Loc.t -> int -> endianness option -> expr option -> blb_fmt
(** [blb_fmt_int loc width endian expr] constructs a BLOB format integer
    interpolation with width [width], endianness [endian], and optional value
    expression [expr] at location [loc]. *)

val blb_fmt_float : Loc.t -> expr option -> blb_fmt
(** [blb_fmt_float loc expr] constructs a BLOB format floating-point
    interpolation with optional value expression [expr] at location [loc]. *)

val blb_fmt_rune : Loc.t -> endianness option -> expr option -> blb_fmt
(** [blb_fmt_rune loc endian expr] constructs a BLOB format rune interpolation
    with the endianness [endian] and optional value expression [expr] at
    location [loc]. *)

val blb_fmt_string : Loc.t -> endianness option -> expr option -> blb_fmt
(** [blb_fmt_string loc endian expr] constructs a BLOB format string
    interpolation with the endianness [endian] and optional value expression
    [expr] at location [loc]. *)

val blb_fmt_byte : Loc.t -> expr option -> blb_fmt
(** [blb_fmt_byte loc expr] constructs a BLOB format byte interpolation with
    optional value expression [expr] at location [loc]. *)

val blb_fmt_blob : Loc.t -> expr option -> blb_fmt
(** [blb_fmt_blob loc expr] constructs a BLOB format BLOB interpolation with
    optional value expression [expr] at location [loc]. *)

(** {3 Format Timestamps} *)

val ts_fmt_const : Loc.t -> Uchar.t list -> ts_fmt
(** [ts_fmt_const loc str] constructs a constant timestamp format component at
    location [loc] with value [str]. *)

val ts_fmt_pct : Loc.t -> wp -> ts_fmt
(** [ts_fmt_pct loc width] constructs a timestamp format percent interpolation
    at location [loc] with width [width]. *)

val ts_fmt_week_day : Loc.t -> wp -> expr option -> bool -> bool -> bool -> ts_fmt
(** [ts_fmt_week_day loc width expr named monday full] constructs a timestamp
    format week day interpolation at [loc] with width [width] and optional value
    expression [expr].  If [named] is true, use the week day name, otherwise use
    the number.  If [monday] is true, then Monday is considered the first day of
    the week, otherwise Sunday is.  If [full] is true, the full week day name is
    used, otherwise it is abbreviated. *)

val ts_fmt_month_day : Loc.t -> wp -> expr option -> bool -> ts_fmt
(** [ts_fmt_month_day loc width expr pad] constructs a timestamp format month
    day interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [pad] is true, then the day is padded to two digits
    with leading zeros. *)

val ts_fmt_month : Loc.t -> wp -> expr option -> bool -> bool -> ts_fmt
(** [ts_fmt_month loc width expr named full] constructs a timestamp format month
    interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [named] is true, then the month name is used,
    otherwise the month number is used.  If [full] is true, then the full month
    name is used or the month number is padded to two digits with leading zeros,
    otherwise the abbreviated month name is used or the month number is not
    padded. *)

val ts_fmt_year : Loc.t -> wp -> expr option -> bool -> bool -> ts_fmt
(** [ts_fmt_year loc width expr century pad] constructs a timestamp format year
    interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [century] is true, then the century is displayed.  If
    [pad] is true, then pad the year to two (or four, if [century] is true)
    digits with leading zeros. *)

val ts_fmt_hour : Loc.t -> wp -> expr option -> bool -> bool -> ts_fmt
(** [ts_fmt_hour loc width expr twenty_four pad] constructs a timestamp format
    hour interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [twenty_four] is true, then a 24-hour clock is used.
    If [pad] is true, the hour is padded to two digits with leading zeros. *)

val ts_fmt_am_pm : Loc.t -> wp -> expr option -> bool -> ts_fmt
(** [ts_fmt_am_pm loc width expr capital] constructs a timestamp format AM/PM
    interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [capital] is true, then a capital AM or PM value is
    used, otherwise a lower-case value is used. *)

val ts_fmt_minute : Loc.t -> wp -> expr option -> bool -> ts_fmt
(** [ts_fmt_minute loc width expr pad] constructs a timestamp format minute
    interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [pad] is true, then the minute is padded to two
    digits with leading zeros. *)

val ts_fmt_second : Loc.t -> wp -> expr option -> bool -> ts_fmt
(** [ts_fmt_second loc width expr pad] constructs a timestamp format second
    interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [pad] is true, then the minute is padded to two
    digits with leading zeros. *)

val ts_fmt_subsecond : Loc.t -> wp -> wp -> expr option -> ts_fmt
(** [ts_fmt_subsecond loc width precision expr] constructs a timestamp format
    sub-second interpolation at location [loc] with width [width], precision
    [precision], and optional value expression [expr]. *)

val ts_fmt_zone : Loc.t -> ts_fmt (** TODO *)

val ts_fmt_year_day : Loc.t -> wp -> expr option -> bool -> ts_fmt
(** [ts_fmt_year_day loc width expr pad] constructs a timestamp format day of
    year interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [pad] is true, then the day is padded to three
    digits with leading zeros. *)

val ts_fmt_year_week : Loc.t -> wp -> expr option -> bool -> ts_fmt
(** [ts_fmt_year_week loc width expr pad] constructs a timestamp format week of
    year interpolation at location [loc] with width [width] and optional value
    expression [expr].  If [pad] is true, then the week is padded to two
    digits with leading zeros. *)

(** {3 Top-Level Statements} *)

val top_val : Loc.t -> binding -> top
(** [top_val loc b] constructs a top-level value binding expression at location
    [loc] binding [b].  The location should span from the initial "val" keyword
    to [b]. *)

val top_def : Loc.t -> binding -> top
(** [top_def loc b] constructs a top-level function binding expression at
    location [loc] binding [b].  The location should span from the initial "def"
    keyword to [b]. *)

val top_type : Loc.t -> Sym.sym -> Type.t -> top
(** [top_type loc id ty] constructs a top-level type definition at location
    [loc] binding the type [ty] to the name [id].  The location should span from
    the initial "type" keyword to the end of [ty]. *)

(** {3 Import Statements} *)

val name : Loc.t -> string -> name
(** [name loc id] constructs a name at location [loc] with the name [id]. *)

val version : Loc.t -> int -> version
(** [version loc v] constructs a major version number at location [loc] with
    version [v]. *)

val src : Loc.t -> name -> version -> src
(** [source loc mojule version] constructs a package source at location [loc]
    importing from module [mojule] major version [version]. *)

val from : Loc.t -> src option -> from
(** [from loc src] constucts a from clause at location [loc] importing from the
    source module [src].  If [src] is given, it specifies an external package to
    import from.  Otherwise, it specifies the current package. *)

val alias : Loc.t -> name -> name option -> alias
(** [alias package alias] constructs an alias clause at location [loc] with
    package name [package] and an optional alias [alias].  If no local alias is
    given, the module's declared name is used. *)

val pkgs : Loc.t -> alias list -> pkgs
(** [pkgs loc clauses] constructs a set of module imports at location [loc]
    importing packages [clauses]. *)

val import : Loc.t -> from option -> pkgs -> import
(** [import loc from pkgs] constructs an import statement at location [loc]
    importing [pkgs] from the module [from]. *)

(** {3 Package Statements} *)

val pkg : Loc.t -> name -> pkg
(** [pkg loc name] constructs a package statement at location [loc] declaring
    the name of the package to be [name]. *)

(** {3 Source Files} *)

val file : pkg -> import list -> top list -> file
(** [file pkg imports tops] constructs a source file in the package [pkg],
    importing the packages [imports] containing the list of top-level bindings
    [tops]. *)

(** {2 Operations}
 *
 * Common operations on Abstract Syntax Trees.
 *)

val precedence : expr -> int
(** [precedence expr] returns the precedence of the express [expr]. *)

val loc_expr : expr -> Loc.t
(** [loc_expr expr] returns the location information associated with the
    expression [expr]. *)

val loc_top : top -> Loc.t
(** [loc_top expr] returns the location information associated with the
    top-level binding [top]. *)
