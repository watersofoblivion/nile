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

%token <Loc.t> LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COLON ARROW DARROW BIND COMMA GROUND PIPE ELIPSIS
%token <Loc.t> PACKAGE IMPORT FROM
%token <Loc.t> TYPE VAL DEF LET REC AND IN
%token <Loc.t> IF THEN ELSE
%token <Loc.t> CASE OF END
%token <Loc.t> AS
%token <Loc.t> ADD SUB MUL DIV MOD
%token <Loc.t> LAND LOR LNOT
%token <Loc.t> EQ NEQ
%token <Loc.t> LTE LT GT GTE
%token <Loc.t> DOT
%token <Loc.t> CONS
%token <Loc.t> EOF
%token <Loc.t> UNIT
%token <Loc.t> STRUCT
%token <Loc.t * int> INT
%token <Loc.t * bool> BOOL
%token <Loc.t * float> FLOAT
%token <Loc.t * string> STRING BLOB TIMESTAMP DURATION
%token <Loc.t * string> SRC
%token <Loc.t * string> LIDENT UIDENT

%left  IN
%left  ELSE
%left  LOR
%left  LAND
%left  EQ NEQ
%left  LTE LT GT GTE
%right CONS
%left  ADD SUB
%left  MUL DIV MOD
%right LNOT
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

compound_id:
  simple_id DOT compound_id { $1 :: $3 }
| simple_id                 { $1 :: [] }
;

simple_id:
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
  COLON ty { $2 }
;

ty:
  compound_id                                  { make_prim_ty $1 }
| ty ARROW ty                                  { make_fun_ty $1 $3 }
| LPAREN ty COMMA tuple_ty RPAREN              { make_tuple_ty $1 ($2 :: $4) $5 }
| STRUCT LBRACE opt_record_ty opt_comma RBRACE { make_record_ty $1 $3 $4 }
| LPAREN ty RPAREN                             { $2 }
;

tuple_ty:
  ty COMMA tuple_ty { $1 :: $3 }
| ty                { $1 :: [] }
;

opt_record_ty:
  record_ty { $1 }
|           { [] }
;

record_ty:
  field_ty COMMA record_ty { $1 :: $3 }
| field_ty                 { $1 :: [] }
;

field_ty:
  simple_id COLON ty { make_field_ty $1 $3 }
;

opt_comma:
  COMMA { () }
|       { () }
;

/************
 * Patterns *
 ************/

opt_pattern:
  pattern { Some $1 }
|         { None }
;

pattern:
| GROUND                                       { make_ground_patt }
| INT                                          { make_int_patt $1 }
| BOOL                                         { make_bool_patt $1 }
| FLOAT                                        { make_float_patt $1 }
| STRING                                       { make_string_patt $1 }
| LIDENT                                       { make_var_patt $1 }
| LPAREN pattern COMMA tuple_pattern RPAREN    { make_tuple_patt $1 ($2 :: $4) $5 }
| LBRACE field_pattern_list opt_elipsis RBRACE { make_record_patt $1 $2 $3 $4 }
| compound_id opt_pattern                      { make_constr_patt $1 $2 }
| pattern PIPE or_pattern                      { make_or_pattern ($1 :: $3) }
;

tuple_pattern:
  pattern COMMA tuple_pattern { $1 :: $3 }
| pattern                     { $1 :: [] }
;

field_pattern_list:
  field_pattern COMMA field_pattern_list { $1 :: $3 }
| field_pattern                          { $1 :: [] }
;

field_pattern:
  simple_id COLON pattern { make_named_field_patt $1 $3 }
| simple_id               { make_bare_field_patt $1 }
;

opt_elipsis:
  COMMA ELIPSIS { true }
|               { false }
;

or_pattern:
  pattern PIPE or_pattern { $1 :: $3 }
| pattern                 { $1 :: [] }
;

/*******************
 * Parameter Lists *
 *******************/

opt_params_list:
  LPAREN params_list RPAREN { $2 }
|                           { [] }
;

params_list:
  param COMMA params_list { $1 :: $3 }
| param                   { [$1] }
;

param:
  LIDENT annotation { ($1, $2) }
;

/************
 * Bindings *
 ************/

bindings:
  rec_binding AND bindings { $1 :: $3 }
| rec_binding              { [$1] }
;

rec_binding:
  LIDENT LPAREN params_list RPAREN annotation BIND term { make_binding $1 $2 (Some $3) $5 }
;

binding:
  LIDENT opt_params_list opt_annotation BIND term { make_binding $1 $2 $3 $5 }
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
  PACKAGE LIDENT { make_pkg $1 $2 }
;

import_list:
  import_stmt import_list { $1 :: $2 }
|                         { [] }
;

import_stmt:
  from IMPORT import_clauses { make_import $1 $3 }
;

from:
  FROM SRC { Some $2 }
|          { None }
;

import_clauses:
  alias_clause import_clauses_rest { $1 :: $2 }
| import_clauses_rest              { $1 }
;

import_clauses_rest:
  PIPE alias_clause import_clauses_rest { $2 :: $3 }
|                                        { [] }
;

alias_clause:
  LIDENT ARROW STRING { (Some $1, $3) }
| STRING              { (None, $1) }
;

top_list:
  top top_list { $1 :: $2 }
|              { [] }
;

top:
  TYPE simple_id BIND ty                                           { make_top_val $1 $2 }
| VAL simple_id opt_annotation BIND term                           { make_top_val $1 $2 }
| DEF simple_id LPAREN params_list RPAREN opt_annotation BIND term { make_top_def $1 $2 }
| LET binding                                                      { make_top_let $1 $2 }
| LET REC bindings                                                 { make_top_let_rec $1 $3 }
;

/***************
 * Expressions *
 ***************/

/* Exposed for unit testing only */
unit_test:
  term EOF { $1 }
;

term:
  LET binding IN term                                  { make_bind $1 $2 $4 }
| LET REC bindings IN term                             { make_bind_rec $1 $3 $5 }
| IF app THEN term ELSE term                           { make_cond $1 $2 $4 $6 }
| CASE app OF clauses END                              { make_case_of $2 $4 }
| LPAREN params_list RPAREN opt_annotation DARROW term { make_abs $2 $4 $6 }
| app                                                  { $1 }
;

clauses:
  PIPE clause_list { $2 }
| clause_list      { $1 }
|                  { [] }
;

clause_list:
  clause PIPE clause_list { $1 :: $3 }
|                         { [] }
;

clause:
  pattern ARROW term { make_clause $1 $3 }
;

app:
  app atom     { make_app $1 $2 }
| app LOR app  { make_bin_op $1 Op.bin_or  $3 }
| app LAND app { make_bin_op $1 Op.bin_and $3 }
| app EQ app   { make_bin_op $1 Op.bin_eq  $3 }
| app NEQ app  { make_bin_op $1 Op.bin_neq $3 }
| app LTE app  { make_bin_op $1 Op.bin_lte $3 }
| app LT app   { make_bin_op $1 Op.bin_lt  $3 }
| app GT app   { make_bin_op $1 Op.bin_gt  $3 }
| app GTE app  { make_bin_op $1 Op.bin_gte $3 }
| app ADD app  { make_bin_op $1 Op.bin_add $3 }
| app SUB app  { make_bin_op $1 Op.bin_sub $3 }
| app MUL app  { make_bin_op $1 Op.bin_mul $3 }
| app DIV app  { make_bin_op $1 Op.bin_div $3 }
| app MOD app  { make_bin_op $1 Op.bin_mod $3 }
| LNOT app     { make_un_op  $1 Op.un_not  $2 }
| app DOT app  { make_bin_op $1 Op.bin_dot $3 }
| compound_id LBRACE opt_field_list RBRACE { make_record $1 $3 $4 }
| atom         { $1 }
;

atom:
  UNIT                           { make_unit $1 }
| BOOL                           { make_bool $1 }
| INT                            { make_int $1 }
| FLOAT                          { make_float $1 }
| STRING                         { make_string $1 }
| BLOB                           { make_blob $1 }
| TIMESTAMP                      { make_timestamp $1 }
| DURATION                       { make_duration $1 }
| simple_id                      { make_var $1 }
| LPAREN term COMMA tuple RPAREN { make_tuple $1 ($2 :: $4) $5 }
| LPAREN term RPAREN             { $2 }
;

tuple:
  term COMMA tuple { $1 :: $3 }
| term             { $1 :: [] }
;

opt_field_list:
  field_list { $1 }
|            { [] }
;

field_list:
  field COMMA field_list { $1 :: $3 }
| field                  { $1 :: [] }
;

field:
  simple_id COLON term { make_field $1 $3 }
;
