open Common

(** {1 Abstract Syntax}
 *
 * The external syntax of the language.
 *)

(** {2 Syntax}
 *
 * Data types representing all of the various constructs in the external syntax.
 *)

(** {3 Expressions} *)

type expr = private
  | Unit of {
      loc: Loc.t; (** Location Tracking Information *)
    } (** Unit *)
  | Bool of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Boolean *)
  | Int of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      radix:  int;    (** Radix *)
    } (** Integer *)
  | Float of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      hex:    bool    (** Is Hex Notation *)
    } (** Floating-point *)
  | Rune of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
    } (** Rune *)
  | String of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** String *)
  | Byte of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Byte *)
  | Blob of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Binary Large Object (BLOB) *)
  | Timestamp of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** ISO-8601 Timestamp *)
  | Duration of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
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
      loc: Loc.t; (** Location Tracking Information *)
      op:  Op.un; (** Operator *)
      rhs: expr   (** Right-Hand Side *)
    } (** Unary Operation *)
  | BinOp of {
      loc: Loc.t;  (** Location Tracking Information *)
      lhs: expr;   (** Left-Hand Side *)
      op:  Op.bin; (** Operator *)
      rhs: expr    (** Right-Hand Side *)
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
  | Let of {
      loc:     Loc.t;   (** Location Tracking Information *)
      binding: binding; (** Binding *)
      scope:   expr     (** Scope of Binding *)
    } (** Value Binding *)
  | LetRec of {
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

(** {3 Top-Level Statements} *)

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

(** {3 Import Statements} *)

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

(** {3 Package Statements} *)

type pkg = Package of {
  loc:  Loc.t; (** Location Tracking Information *)
  name: name   (** Package Name *)
}
(** Package statement declaring the name of the package this file is in. *)

(** {3 Source Files} *)

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

(** {3 Expressions} *)

val unit : Loc.t -> expr
(** [unit loc] constructs a unit literal at location [loc]. *)

val bool : Loc.t -> bool -> expr
(** [bool loc lexeme] constructs a boolean literal with the lexeme [lexeme] at
    location [loc]. *)

val int : Loc.t -> string -> int -> expr
(** [int loc lexeme radix] constructs an integer literal with the lexeme
    [lexeme] and radix [radix] at location [loc].  The lexeme should include the
    [0<radix>] prefix, if present. *)

val float : Loc.t -> string -> bool -> expr
(** [float loc lexeme hex] constructs a floating-point literal with the lexeme
    [lexeme] at location [loc].  If [hex] is true, then the lexeme is in
    hexadecimal form.  The lexeme should include the [0f] prefix, if present. *)

val rune : Loc.t -> string -> expr
(** [rune loc lexeme] constructs a rune literal with the lexeme [lexeme] at
    location [loc].  The lexeme should include the opening and closing single
    quotes. *)

val string : Loc.t -> string -> expr
(** [string loc lexeme] constructs a string literal with the lexeme [lexeme] at
    location [loc].  The lexeme should include the opening and closing double
    quotes. *)

val byte : Loc.t -> string -> expr
(** [byte loc lexeme] constructs a byte literal with the lexeme [lexeme] at
    location [loc].  The lexeme should include the [\<radix>] prefix. *)

val blob : Loc.t -> string -> expr
(** [blob loc lexeme] constructs a binary large object (BLOB) literal with the
    lexeme [lexeme] at location [loc].  The lexeme should include the back
    quotes. *)

val timestamp : Loc.t -> string -> expr
(** [timestamp loc lexeme] constructs a ISO-8601 timestamp literal with the
    lexeme [lexeme] at location [loc].  The lexeme should include the [@]
    prefix. *)

val duration : Loc.t -> string -> expr
(** [duration loc lexeme] constructs a ISO-8601 duration literal with the lexeme
    [lexeme] at location [loc].  The lexeme should include the [@@] prefix. *)

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

val un_op : Loc.t -> Op.t -> int -> expr -> expr
(** [un_op loc op prec r] constructs a unary operator expression at location [loc]
    with the operator [op] of precedence [prec] operating on [r].  The location
    should span from [op] to [r]. *)

val bin_op : Loc.t -> Op.t -> int -> expr -> expr -> expr
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
