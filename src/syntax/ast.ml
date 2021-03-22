open Common

(* Syntax *)

type radix =
  | Binary
  | Octal
  | Decimal
  | Hex
type wp =
  | Natural
  | Static of { min: int }
  | Dynamic
type justification =
  | Left
  | Right
type signedness =
  | Unsigned
  | Signed
  | Positive
  | Space
type endianness =
  | Big
  | Little
type expr =
  | Unit of { loc: Loc.t }
  | Bool of { loc: Loc.t; b: bool }
  | Int of { loc: Loc.t; lexeme: string; radix: radix; i: Z.t }
  | Float of { loc: Loc.t; lexeme: string; hex: bool; q: Q.t }
  | Rune of { loc: Loc.t; r: Uchar.t; size: int }
  | String of { loc: Loc.t; fmt: fmt list }
  | Byte of { loc: Loc.t; lexeme: string; radix: radix; b: bytes }
  | Blob of { loc: Loc.t; endian: endianness option; fmt: blb_fmt list }
  | Timestamp of { loc: Loc.t; extended: bool; year: expr option; month: expr option; week: expr option; dow: expr option; day: expr option; ordinal: expr option; hour: expr option; minute: expr option; second: expr option; fraction: expr option; utc: bool; off_sign: bool; off_hour: expr option; off_minute: expr option }
  | Duration of { loc: Loc.t; extended: bool; years: expr option; months: expr option; weeks: expr option; days: expr option; hours: expr option; minutes: expr option; seconds: expr option; fraction: expr option }
  | Tuple of { loc: Loc.t; exprs: expr list }
  | Record of { loc: Loc.t; constr: Sym.sym option; fields: field list }
  | Var of { loc: Loc.t; id: Sym.sym }
  | UnOp of { loc: Loc.t; op: Sym.sym; prec: int; rhs: expr }
  | BinOp of { loc: Loc.t; op: Sym.sym; prec: int; lhs: expr; rhs: expr }
  | Slice of { loc: Loc.t; expr: t; start: t option; stop: t option }
  | Index of { loc: Loc.t; expr: t; idx: t }
  | If of { loc: Loc.t; cond: expr; tru: expr; fls: expr }
  | Case of { loc: Loc.t; scrut: expr; clauses: clause list }
  | Bind of { loc: Loc.t; binding: binding; scope: expr }
  | BindRec of { loc: Loc.t; bindings: binding list; scope: expr }
  | Abs of { loc: Loc.t; params: param list; res: Type.t option; body: expr }
  | App of { loc: Loc.t; fn: expr; args: expr list }
and field = Field of { loc: Loc.t; name: Sym.sym; value: expr }
and param = Param of { loc: Loc.t; name: Patt.t; ty: Type.t }
and binding = Binding of { loc: Loc.t; name: Patt.t; ty: Type.t option; body: expr }
and clause = Clause of { loc: Loc.t; patt: Patt.t; body: expr }
and fmt =
  | FmtConst of { loc: Loc.t; str: Uchar.t list; len: int; size: int }
  | FmtPct of { loc: Loc.t; width: wp; }
  | FmtBool of { loc: Loc.t; width: wp; expr: expr option }
  | FmtInt of { loc: Loc.t; width: wp; discrim: Uchar.t; radix: radix; expr: expr option; signedness: signedness; justification: justification; capital: bool; alt: bool }
  | FmtFloat of { loc: Loc.t; width: wp; precision: wp; expr: expr option; signedness: signedness; justification: justification }
  | FmtRune of { loc: Loc.t; width: wp; discrim: Uchar.t; expr: expr option; quote: bool; justification: justification }
  | FmtString of { loc: Loc.t; width: wp; expr: expr option; quote: bool; justification: justification }
and blb_fmt =
  | BlbFmtConst of { loc: Loc.t; lexeme: string; bs: bytes; size: int }
  | BlbFmtBool of { loc: Loc.t; width: int; endian: endianness option; expr: expr option }
  | BlbFmtInt of { loc: Loc.t; width: int; endian: endianness option; expr: expr option }
  | BlbFmtFloat of { loc: Loc.t; expr: expr option }
  | BlbFmtRune of { loc: Loc.t; endian: endianness option; expr: expr option }
  | BlbFmtString of { loc: Loc.t; endian: endianness option; expr: expr option }
  | BlbFmtByte of { loc: Loc.t; expr: expr option }
  | BlbFmtBlob of { loc: Loc.t; expr: expr option }
and ts_fmt =
  | TsFmtConst of { loc: Loc.t; str: Uchar.t list; len: int; size: int }
  | TsFmtPct of { loc: Loc.t; width: wp }
  | TsFmtWeekDay of { loc: Loc.t; width: wp; expr: expr option; named: bool; monday: bool; full: bool }
  | TsFmtMonthDay of { loc: Loc.t; width: wp; expr: expr option; pad: bool }
  | TsFmtMonth of { loc: Loc.t; width: wp; expr: expr option; named: bool; full: bool }
  | TsFmtYear of { loc: Loc.t; width: wp; expr: expr option; century: bool; pad: bool }
  | TsFmtHour of { loc: Loc.t; width: wp; expr: expr option; twenty_four: bool; pad: bool }
  | TsFmtAmPm of { loc: Loc.t; width: wp; expr: expr option; capital: bool }
  | TsFmtMinute of { loc: Loc.t; width: wp; expr: expr option; pad: bool }
  | TsFmtSecond of { loc: Loc.t; width: wp; expr: expr option; pad: bool }
  | TsFmtSubsecond of { loc: Loc.t; width: wp; precision: wp; expr: expr option }

type top =
  | Val of { loc: Loc.t; binding: binding }
  | Def of { loc: Loc.t; binding: binding }
  | Type of { loc: Loc.t; name: Sym.sym; defn: Type.t }

type name = Name of { loc: Loc.t; name: Sym.sym }
type version = Version of { loc: Loc.t; version: int }
type src = Source of { loc: Loc.t; mojule: name; version: version }
type from = From of { loc: Loc.t; source: src option }
type alias = Alias of { loc: Loc.t; package: name; alias: name option }
type pkgs = Packages of { loc: Loc.t; clauses: alias list }
type import = Import of { loc: Loc.t; from: from option; package: pkgs }

type pkg = Package of { loc: Loc.t; name: name }

type file = File of { package: pkg; imports: import list; tops: top list }

(* Constructors *)

let binary = Binary
let octal = Octal
let decimal = Decimal
let hex = Hex

let natural = Natural
let static min = Static { min }
let dynamic = Dynamic

let left = Left
let right = Right

let unsigned = Unsigned
let signed = Signed
let positive = Positive
let space = Space

let big = Big
let little = Little

let unit loc = Unit { loc }
let bool loc b = Bool { loc; b }
let int loc lexeme radix i = Int { loc; lexeme; radix; i }
let float loc lexeme hex q = Float { loc; lexeme; hex; q }
let rune loc r = Rune { loc; r }
let string loc fmt = String { loc; fmt }
let byte loc lexeme = Byte { loc; lexeme }
let blob loc lexeme = Blob { loc; lexeme }
let timestamp loc extended year month week dow day ordinal hour minute second fraction utc off_hour off_minute = Timestamp { loc; extended; year; month; week; dow; day; ordinal; hour; minute; second; fraction; utc; off_hour; off_minute }
let duration loc extended years months weeks days hours minutes seconds fraction = Duration { loc; extended; years; months; weeks; days; hours; minutes; seconds; fraction }
let tuple loc exprs = Tuple { loc; exprs }
let record loc constr fields = Record { loc; constr; fields }
let var loc id = Var { loc; id }
let un_op loc op prec rhs = UnOp { loc; op; prec; r }
let bin_op loc op prec lhs rhs = BinOp { loc; op; prec; lhs; rhs }
let slice loc expr start stop = Slice { loc; expr; start; stop }
let index loc expr idx = Index { loc; expr; idx }
let cond loc cond tru fls = If { loc; cond; tru; fls }
let case loc scrut clauses = Case { loc; scrut; clauses }
let bind loc binding scope = Bind { loc; binding; scope }
let bind_rec loc bindings rest = BindRec { loc; bindings; scope }
let abs loc params res body = Abs { loc; params; res; body }
let app loc fn args = App { loc; fn; args }

let field loc name value = Field { loc; name; value }
let param loc name ty = Param { loc; name; ty }
let binding loc name ty expr = Binding { loc; name; ty; expr }
let clause loc patt expr = Clause { loc; patt; expr }

let fmt_const loc str = FmtConst { loc; str }
let fmt_pct loc width = FmtPct { loc; width }
let fmt_bool loc width expr = FmtBool { loc; width; expr }
let fmt_int loc width discrim radix expr signedness justification capital alt = FmtInt { loc; width; discrim; radix; expr; signedness; justification; capital; alt }
let fmt_float loc width precision expr signedness justification = FmtFloat { loc; width; precision; expr; signedness; justification }
let fmt_rune loc width discrim expr quote justification = FmtRune { loc; width; discrim; expr; quote; justification }
let fmt_string loc width discrim expr quote justification = FmtString { loc; width; discrim; expr; quote; justification }

let blb_fmt_const loc lexeme bs = BlbFmtConst { loc; lexeme; bs; size = Bytes.length bs }
let blb_fmt_bool loc width endian expr = BlbFmtBool { loc; width; endian; expr }
let blb_fmt_int loc width endian expr = BlbFmtInt { loc; width; endian; expr }
let blb_fmt_float loc expr = BlbFmtFloat { loc; expr }
let blb_fmt_rune loc endian expr = BlbFmtRune { loc; endian; expr }
let blb_fmt_string loc endian expr = BlbFmtString { loc; endian; expr }
let blb_fmt_byte loc expr = BlbFmtByte { loc; expr }
let blb_fmt_blob loc expr = BlbFmtBlob { loc; expr }

let ts_fmt_const loc str = TsFmtConst { loc; str }
let ts_fmt_pct loc width = TsFmtPct { loc; width }
let ts_fmt_week_day loc width expr named monday full = TsFmtWeekDay { loc; width; expr; named; monday; full }
let ts_fmt_month_day loc width expr pad = TsFmtMonthDay { loc; width; expr; pad }
let ts_fmt_month loc width expr named full = TsFmtMonth { loc; width; expr; named; full }
let ts_fmt_year loc width expr century pad = TsFmtYear { loc; width; expr; century; pad }
let ts_fmt_hour loc width expr twenty_four pad = TsFmtHour { loc; width; expr; twenty_four; pad }
let ts_fmt_am_pm loc width expr capital = TsFmtAmPm { loc; width; expr; capital }
let ts_fmt_minute loc width expr pad = TsFmtMinute { loc; width; expr; pad }
let ts_fmt_second loc width expr pad = TsFmtSecond { loc; width; expr; pad }
let ts_fmt_subsecond loc width precision expr = TsFmtSubsecond { loc; width; precision; expr }
let ts_fmt_zone loc = TsFmtZone { loc }
let ts_fmt_year_day loc width expr pad = TsFmtYearDay { loc; width; expr; pad }
let ts_fmt_year_week loc width expr pad = TsFmtYearWeek { loc; width; expr; pad }

let top_val loc binding = Val { loc; binding }
let top_def loc binding = Def { loc; binding }
let top_type loc name ty = Type { loc; name; ty }

let name loc id = Name { loc; id }
let version loc major = Version { loc; major }
let src loc mojule version = Source { loc; mojule; version }
let from loc src = From { loc; src }
let alias loc package alias = Alias { loc; package; alias }
let pkgs loc clauses = Packages { loc; clauses }
let import loc from packages = Import { loc; from; packages }

let pkg loc name = Package { loc; name }

let file package imports tops = File { package; imports; tops }

(* Operations *)

let precedence = function
  | Unit _ | Bool _ | Int _ | Float _ | Rune _ | String _ | Byte _ | Blob _ | Timestamp _ | Duration _
  | Tuple _ | Record _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ | Case _ -> 13
  | Abs _ -> 14
  | Bind _ | BindRec _ -> 15

let loc_expr = function
  | Unit expr -> expr.loc
  | Bool expr -> expr.loc
  | Int expr -> expr.loc
  | Float expr -> expr.loc
  | Rune expr -> expr.loc
  | String expr -> expr.loc
  | Byte expr -> expr.loc
  | Blob expr -> expr.loc
  | Timestamp expr -> expr.loc
  | Duration expr -> expr.loc
  | Tuple expr -> expr.loc
  | Record expr -> expr.loc
  | UnOp expr -> expr.loc
  | BinOp expr -> expr.loc
  | Slice expr -> expr.loc
  | Index expr -> expr.loc
  | If expr -> expr.loc
  | Case expr -> expr.loc
  | Bind expr -> expr.loc
  | BindRec expr -> expr.loc
  | Abs expr -> expr.loc
  | App expr -> expr.loc
  | Var expr -> expr.loc

let loc_top = function
  | Val top -> top.loc
  | Def top -> top.loc
  | Type top -> top.loc
