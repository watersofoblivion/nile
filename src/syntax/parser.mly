%{
  open Format
  open Common

  (* Types *)

  let make_unit_ty loc tbl kontinue =
    Type.unit loc
      |> kontinue tbl

  let make_bool_ty loc tbl kontinue =
    Type.bool loc
      |> kontinue tbl

  let make_int_ty loc tbl kontinue =
    Type.int loc
      |> kontinue tbl

  let make_float_ty loc tbl kontinue =
    Type.float loc
      |> kontinue tbl

  let make_rune_ty loc tbl kontinue =
    Type.rune loc
      |> kontinue tbl

  let make_string_ty loc tbl kontinue =
    Type.string loc
      |> kontinue tbl

  let make_blob_ty loc tbl kontinue =
    Type.blob loc
      |> kontinue tbl

  let make_timestamp_ty loc tbl kontinue =
    Type.timestamp loc
      |> kontinue tbl

  let make_duration_ty loc tbl kontinue =
    Type.duration loc
      |> kontinue tbl

  let make_fun_ty a b tbl kontinue =
    a tbl (fun tbl a ->
      b tbl (fun tbl b ->
        Common.Type.func a b
          |> kontinue tbl))

  let make_tuple_ty lparen tys rparen tbl kontinue =

  let make_field_ty (loc, id) ty tbl kontinue =
    let (id, tbl) = Sym.symbolize id tbl in
    ty tbl (fun tbl ty ->
      Type.field loc id ty tbl
        |> kontinue tbl)

  let make_record_ty strct fields rbrace tbl kontinue =
    Type.record loc fields
      |> kontinue tbl

  let make_variant_ty constrs tbl kontinue =

  let make_constr_ty (loc, id) ty tbl kontinue =
    let (id, tbl) = Sym.symbolize id tbl in
    match ty with
      | None ->
        Type.constr loc id ty
          |> kontinue tbl
      | Some ty ->
        ty tbl (fun tbl ty ->
          Type.constr loc id ty
            |> kontinue tbl)

  (* Patterns *)

  let make_ground_patt loc tbl kontinue =
    Patt.ground loc
      |> kontinue tbl

  let make_nil_patt loc tbl kontinue =
    Patt.nil loc
      |> kontinue tbl

  let make_unit_patt loc tbl kontinue =
    Patt.unit loc
      |> kontinue tbl

  let make_bool_patt (loc, b) tbl kontinue =
    Patt.bool loc b
      |> kontinue tbl

  let make_int_patt (loc, i) tbl kontinue =
    Patt.int loc i
      |> kontinue tbl

  let make_float_patt (loc, f) tbl kontinue =
    Patt.float loc f
      |> kontinue tbl

  let make_rune_patt (loc, r) tbl kontinue =
    Patt.rune loc r
      |> kontinue tbl

  let make_string_patt (loc, s) tbl kontinue =
    Patt.string loc s
      |> kontinue tbl

  let make_var_patt (loc, id) tbl kontinue =
    let (sym, tbl) = Sym.symbolize id tbl in
    Patt.var loc id
      |> kontinue tbl

  let make_tuple_patt lparen patts rparen tbl kontinue =

  let make_named_field_patt id patt tbl kontinue =

  let make_bare_field_att id tbl kontinue =

  let make_record_patt lbrace fields elipsis rbrace tbl kontinue =

  let make_constr_patt id value tbl kontinue =

  let make_or_patt patts tbl kontinue =

  (* Atoms *)

  let make_unit loc tbl kontinue =
    Ast.unit loc
      |> kontinue tbl

  let make_bool (loc, b) tbl kontinue =
    Ast.bool loc b
      |> kontinue tbl

  let make_int (loc, i) tbl kontinue =
    Ast.int loc i
      |> kontinue tbl

  let make_float (loc, f) tbl kontinue =
    Ast.float loc f
      |> kontinue tbl

  let make_rune (loc, r) tbl kontinue =
    Ast.rune loc r
      |> kontinue tbl

  let make_string (loc, s) tbl kontinue =
    Ast.string loc s
      |> kontinue tbl

  let make_blob (loc, bs) tbl kontinue =
    Ast.blob loc bs
      |> kontinue tbl

  let make_timestamp (loc, ts) tbl kontinue =
    Ast.timestamp loc ts
      |> kontinue tbl

  let make_duration (loc, d) tbl kontinue =
    Ast.make_duration loc d
      |> kontinue tbl

  let make_var (loc, id) tbl kontinue =
    let (sym, tbl) = Sym.symbolize id tbl in
    Ast.var loc sym
      |> kontinue tbl

  let make_tuple loc exprs tbl kontinue =

  let make_record (loc, constr) fields rbrace tbl kontinue =

  (* Expressions *)

  let make_un_op loc op r tbl kontinue =
    r tbl (fun tbl r ->
      let loc = Loc.span loc (Ast.loc_expr r) in
      Ast.un_op loc op r
        |> kontinue tbl)

  let make_bin_op l op r tbl kontinue =
    l tbl (fun tbl l ->
      r tbl (fun tbl r ->
        let loc = Loc.span (Ast.loc_expr l) (Ast.loc_expr r) in
        Ast.bin_op loc l op r
          |> kontinue tbl))

  let make_cond kwd_loc c t f tbl kontinue =
    c tbl (fun tbl c ->
      t tbl (fun tbl t ->
        f tbl (fun tbl f ->
          let loc = Loc.span kwd_loc (Ast.loc_expr f) in
          Ast.cond loc c t f
            |> kontinue tbl)))

  let make_abs ps ty body tbl kontinue =
    failwith "TODO"

  let make_app f x tbl kontinue =
    f tbl (fun tbl f ->
      x tbl (fun tbl x ->
        let loc = Loc.span (Ast.loc_expr f) (Ast.loc_expr x) in
        Ast.app loc f x
          |> kontinue tbl))

  (* Bindings *)

  let make_bind kwd_loc b rest tbl kontinue =
    b tbl (fun tbl b ->
      rest tbl (fun tbl rest ->
        let loc = Loc.span kwd_loc (Ast.loc_expr rest) in
        Ast.bind loc b rest
          |> kontinue tbl))

  let make_bind_rec kwd_loc bs rest tbl kontinue =
    bs tbl (fun tbl bs ->
      rest tbl (fun tbl rest ->
        let loc = Loc.span kwd_loc (Ast.loc_expr rest) in
        Ast.bind_rec loc bs rest
          |> kontinue tbl))

  let make_binding (loc, id) _ ty expr tbl kontinue =
    let (sym, tbl) = Sym.symbolize id tbl in
    ty tbl (fun tbl ty ->
      expr tbl (fun tbl expr ->
        let loc = Loc.span loc (Ast.loc_expr expr) in
        Ast.binding loc sym (Some ty) expr
          |> kontinue tbl))

  (* Top-Level Bindings *)

  let make_top_let kwd_loc b tbl kontinue =
    b tbl (fun tbl b ->
      Ast.top_bind kwd_loc b
        |> kontinue tbl)

  let make_top_let_rec kwd_loc bs tbl kontinue =
    let fold (tbl, bs) b =
    in
    let (tbl, bs) = List.fold_left fold (tbl, []) bs in
    let bs = List.rev bs in
    bs tbl (fun tbl bs ->
      Ast.top_bind_rec kwd_loc bs
        |> kontinue tbl)

  (* Files *)

  let rec make_file tops tbl kontinue = match tops with
    | [] ->
      tops
        |> List.rev
        |> Ast.file
        |> kontinue tbl
    | top :: tops -> top tbl (fun tbl top -> make_file tops tbl kontinue)
%}

%token <Loc.t> LPAREN "(" RPAREN ")"
%token <Loc.t> LBRACKET "[" RBRACKET "]"
%token <Loc.t> LBRACE "{" RBRACE "}"
%token <Loc.t> COLON ":"
%token <Loc.t> ARROW "->" DARROW "=>"
%token <Loc.t> BIND "="
%token <Loc.t> COMMA ","
%token <Loc.t> GROUND "_"
%token <Loc.t> PIPE "|"
%token <Loc.t> DOT "." ELIPSIS "..."
%token <Loc.t> PACKAGE "package" IMPORT "import" FROM "from"
%token <Loc.t> TYPE "type" VAL "val" DEF "def" LET "let" REC "rec" AND "and" IN "in"
%token <Loc.t> IF "if" THEN "then" ELSE "else"
%token <Loc.t> CASE "case" OF "of" END "end"
%token <Loc.t> AS "as"
%token <Loc.t> BAND "&" BXOR "^" BNOT "~"
%token <Loc.t> LSL "<<" LSR ">>" ASL "<<<" ASR ">>>"
%token <Loc.t> LAND "&&" LOR "||" LNOT "!"
%token <Loc.t> EQ "==" NEQ "!="
%token <Loc.t> ADD "+" SUB "-"
%token <Loc.t> MUL "*" DIV "/" MOD "%"
%token <Loc.t> LTE "<=" LT "<" GT ">" GTE ">="
%token <Loc.t> CONCAT "++"
%token <Loc.t> EOF
%token <Loc.t> STRUCT "struct"
%token <Loc.t> UNIT
%token <Loc.t * string> BOOL RUNE STRING TIMESTAMP DURATION
%token <Loc.t * int * string> INT BYTE BLOB
%token <Loc.t * bool * string> FLOAT
%token <Loc.t * string> SRC
%token <Loc.t * string> LIDENT UIDENT

%left  IN
%left  ELSE
%left  EQ NEQ
%left  LTE LT GT GTE
%right CONS
%left  ADD SUB
%left  MUL DIV MOD
%left  BOR BXOR
%left  BAND
%right LNOT BNOT
%left  DOT
%right ARROW

%type <Sym.t -> (Sym.t -> Ast.file -> (Sym.t -> Ast.file)) -> (Sym.t * Ast.file)> package_only
%type <Sym.t -> (Sym.t -> Ast.file -> (Sym.t -> Ast.file)) -> (Sym.t * Ast.file)> imports_only
%type <Sym.t -> (Sym.t -> Ast.file -> (Sym.t -> Ast.file)) -> (Sym.t * Ast.file)> file
%type <Sym.t -> (Sym.t -> Ast.expr -> (Sym.t * Ast.expr)) -> (Sym.t * Ast.expr)> unit_test

%start package_only
%start imports_only
%start file
%start unit_test

%%

/***************
 * Identifiers *
 ***************/

ident:
  LIDENT { $1 }
| UIDENT { $1 }
;

/*************************
 * Types and Annotations *
 *************************/

opt_annotation:
  annotation { Some $1 }
|            { None }
;

annotation:
  ":" ty { $2 }
;

ty:
  named                                    { make_named_ty $1 }
| ty "->" ty                               { make_fun_ty $1 $3 }
| "(" ty "," tuple_ty ")"                  { make_tuple_ty $1 ($2 :: $4) $5 }
| "struct" "{" opt_record_ty opt_comma "}" { make_record_ty $1 $3 $4 }
| "(" ty ")"                               { $2 }
;

named:
  ident "." named { $1 :: $3 }
| ident           { $1 :: [] }
;

tuple_ty:
  ty "," tuple_ty { $1 :: $3 }
| ty              { $1 :: [] }
;

opt_record_ty:
  record_ty { $1 }
|           { [] }
;

record_ty:
  field_ty "," record_ty { $1 :: $3 }
| field_ty               { $1 :: [] }
;

field_ty:
  ident ":" ty { make_field_ty $1 $3 }
;

opt_comma:
  "," { () }
|     { () }
;

/************
 * Patterns *
 ************/

pattern:
| BOOL                                   { make_bool_patt $1 }
| INT                                    { make_int_patt $1 }
| FLOAT                                  { make_float_patt $1 }
| RUNE                                   { make_rune_patt $1 }
| STRING                                 { make_string_patt $1 }
| BYTE                                   { make_byte_patt $1 }
| BLOB                                   { make_blob_patt $1 }
| "_"                                    { make_ground_patt }
| LIDENT                                 { make_var_patt $1 }
| "(" pattern "," tuple_pattern ")"      { make_tuple_patt $1 ($2 :: $4) $5 }
| "{" field_pattern_list opt_elipsis "}" { make_record_patt $1 $2 $3 $4 }
| constr_pattern pattern_list            { make_constr_patt $1 $2 }
| pattern "|" or_pattern                 { make_or_pattern ($1 :: $3) }
;

tuple_pattern:
  pattern "," tuple_pattern { $1 :: $3 }
| pattern                   { $1 :: [] }
;

field_pattern_list:
  field_pattern "," field_pattern_list { $1 :: $3 }
| field_pattern                        { $1 :: [] }
;

field_pattern:
  ident ":" pattern { make_named_field_patt $1 $3 }
| ident             { make_bare_field_patt $1 }
;

opt_elipsis:
  "," "..." { true }
|           { false }
;

constr_pattern:
  ident "." constr_pattern { $1 :: $3 }
| ident                    { $1 :: [] }
;

pattern_list:
  pattern pattern_list { $1 :: $2 }
|                      { [] }
;

or_pattern:
  pattern "|" or_pattern { $1 :: $3 }
| pattern                { $1 :: [] }
;

/*******************
 * Parameter Lists *
 *******************/

opt_params_list:
  "(" params_list ")" { $2 }
|                     { [] }
;

params_list:
  param "," params_list { $1 :: $3 }
| param                 { [$1] }
;

param:
  LIDENT annotation { ($1, $2) }
;

/************
 * Bindings *
 ************/

bindings:
  rec_binding "and" bindings { $1 :: $3 }
| rec_binding                { [$1] }
;

rec_binding:
  LIDENT "(" params_list ")" annotation "=" term { make_binding $1 $2 (Some $3) $5 }
;

binding:
  LIDENT opt_params_list opt_annotation "=" term { make_binding $1 $2 $3 $5 }
;

/***************
 * Expressions *
 ***************/

/* Exposed for unit testing only */
unit_test:
  term EOF { $1 }
;

term:
  "let" binding "in" term                      { make_bind $1 $2 $4 }
| "let" "rec" bindings "in" term               { make_bind_rec $1 $3 $5 }
| "if" term "then" term "else" term            { make_cond $1 $2 $4 $6 }
| "case" term "of" clauses "end"               { make_case_of $2 $4 }
| "(" params_list ")" opt_annotation "=>" term { make_abs $2 $4 $6 }
| "(" term "," tuple ")"                       { make_tuple $1 ($2 :: $4) $5 }
| ident "{" opt_field_list "}"                 { make_record $1 $3 $4 }
| app                                          { $1 }
;

clauses:
  "|" clause_list { $2 }
| clause_list     { $1 }
|                 { [] }
;

clause_list:
  clause "|" clause_list { $1 :: $3 }
|                        { [] }
;

clause:
  pattern "->" term { make_clause $1 $3 }
;

tuple:
  term "," tuple { $1 :: $3 }
| term           { $1 :: [] }
;

opt_field_list:
  field_list { $1 }
|            { [] }
;

field_list:
  field "," field_list { $1 :: $3 }
| field                { $1 :: [] }
;

field:
  LIDENT ":" term { make_field $1 $3 }
;

app:
  atom atom+     { make_app $1 $2 }
| un_op app      { make_un_op  $1 $2 }
| app bin_op app { make_bin_op $1 $2 $3 }
| atom           { $1 }
;

%inline bin_op:
  "&"   { Op.bin_band }
| "^"   { Op.bin_xor }
| "<<"  { Op.bin_lsl }
| ">>"  { Op.bin_lsr }
| "<<<" { Op.bin_asl }
| ">>>" { Op.bin_asr }
| "+"   { Op.bin_add }
| "-"   { Op.bin_sub }
| "*"   { Op.bin_mul }
| "/"   { Op.bin_div }
| "%"   { Op.bin_mod }
| "=="  { Op.bin_eq }
| "!="  { Op.bin_neq }
| "<="  { Op.bin_lte }
| "<"   { Op.bin_lt }
| ">"   { Op.bin_gt }
| ">="  { Op.bin_gte }
| "++"  { Op.bin_concat }
| "||"  { Op.bin_lor }
| "&&"  { Op.bin_and }
;

%inline un_op:
  "~" { Op.un_bnot }
| "!" { Op.un_lnot }
;

atom:
  UNIT          { make_unit $1 }
| BOOL          { make_bool $1 }
| INT           { make_int $1 }
| FLOAT         { make_float $1 }
| RUNE          { make_rune $1 }
| STRING        { make_string $1 }
| BLOB          { make_blob $1 }
| TIMESTAMP     { make_timestamp $1 }
| DURATION      { make_duration $1 }
| ident         { make_var $1 }
| atom "." atom { make_bin_op $1 Op.bin_dot $3 }
| "(" term ")"  { $2 }
;

/*************
 * Top-Level *
 *************/

package_only:
  package { make_file $1 [] [] }
;

imports_only:
  package import_list { make_file $1 $2 [] }
;

file:
  package import_list top_list EOF { make_file $1 $2 $3 }
;

package:
  "package" LIDENT { make_pkg $1 $2 }
;

import_list:
  import_stmt import_list { $1 :: $2 }
|                         { [] }
;

import_stmt:
  from "import" import_clauses { make_import $1 $3 }
;

from:
  "from" SRC { Some $2 }
|            { None }
;

import_clauses:
  alias_clause import_clauses_rest { $1 :: $2 }
| import_clauses_rest              { $1 }
;

import_clauses_rest:
  "|" alias_clause import_clauses_rest { $2 :: $3 }
|                                      { [] }
;

alias_clause:
  LIDENT "->" STRING { (Some $1, $3) }
| STRING             { (None, $1) }
;

top_list:
  top top_list { $1 :: $2 }
|              { [] }
;

top:
  "type" ident "=" ty                                     { make_top_val $1 $2 }
| "val" ident opt_annotation "=" term                     { make_top_val $1 $2 }
| "def" ident "(" params_list ")" opt_annotation "=" term { make_top_def $1 $2 }
| "let" binding                                           { make_top_let $1 $2 }
| "let" "rec" bindings                                    { make_top_let_rec $1 $3 }
;
