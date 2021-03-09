%{
  open Format
  open Common

  (* Types *)

  let make_prim_ty (_, id) tbl kontinue = match id with
    | "Unit" -> kontinue tbl Common.Type.unit
    | "Int" -> kontinue tbl Common.Type.int
    | "Bool" -> kontinue tbl Common.Type.bool
    | "Float" -> kontinue tbl Common.Type.float
    | "String" -> kontinue tbl Common.Type.string
    | "Blob" -> kontinue tbl Common.Type.blob
    | "Timestamp" -> kontinue tbl Common.Type.timestamp
    | "Duration" -> kontinue tbl Common.Type.duration
    | ty -> failwith (sprintf "Unknown type %S" ty)

  let make_fun_ty a b tbl kontinue =
    a tbl (fun tbl a ->
      b tbl (fun tbl b ->
        Common.Type.func a b
          |> kontinue tbl))

  (* Patterns *)

  let make_ground_patt tbl kontinue =
    Patt.ground
      |> kontinue tbl

  let make_bool_patt (_, b) tbl kontinue =
    Patt.bool b
      |> kontinue tbl

  let make_int_patt (_, i) tbl kontinue =
    Patt.int i
      |> kontinue tbl

  let make_var_patt (_, id) tbl kontinue =
    let (sym, tbl) = Sym.symbolize id tbl in
    Patt.var id
      |> kontinue tbl

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

  let make_var (loc, id) tbl kontinue =
    let (sym, tbl) = Sym.symbolize id tbl in
    Ast.var loc sym
      |> kontinue tbl

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
  UIDENT           { make_prim_ty $1 }
| ty ARROW ty      { make_fun_ty $1 $3 }
| LPAREN ty RPAREN { $2 }
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

/***************
 * Match Cases *
 ***************/

cases:
| PIPE case_list { $2 }
| case_list      { $1 }
|                { [] }
;

case_list:
| case PIPE case_list { $1 :: $3 }
|                     { [] }
;

case:
| pattern ARROW term { ($1, $3) }
;

/************
 * Patterns *
 ************/

pattern:
| GROUND { make_ground_patt }
| INT    { make_int_patt $1 }
| BOOL   { make_bool_patt $1 }
| LIDENT { make_var_patt $1 }
;

/***************
 * Identifiers *
 ***************/

ident:
  LIDENT { $1 }
| UIDENT { $1 }
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
  TYPE ident BIND ty                                           { make_top_val $1 $2 }
| VAL ident opt_annotation BIND term                           { make_top_val $1 $2 }
| DEF ident LPAREN params_list RPAREN opt_annotation BIND term { make_top_def $1 $2 }
| LET binding                                                  { make_top_let $1 $2 }
| LET REC bindings                                             { make_top_let_rec $1 $3 }
;

/***************
 * Expressions *
 ***************/

/* Exposed for unit testing only */
unit_test:
  term EOF { $1 }
;

term:
  app                                                  { $1 }
| LET binding IN term                                  { make_bind $1 $2 $4 }
| LET REC bindings IN term                             { make_bind_rec $1 $3 $5 }
| IF app THEN term ELSE term                           { make_cond $1 $2 $4 $6 }
| CASE app OF cases END                                { make_case_of $2 $4 }
| LPAREN params_list RPAREN opt_annotation DARROW term { make_abs $2 $4 $6 }
;

app:
  app atom     { make_app $1 $2 }
| app LOR app  { make_bin_op $1 Op.bin_or   $3 }
| app LAND app { make_bin_op $1 Op.bin_and  $3 }
| app EQ app   { make_bin_op $1 Op.bin_eq   $3 }
| app NEQ app  { make_bin_op $1 Op.bin_neq  $3 }
| app LTE app  { make_bin_op $1 Op.bin_lte  $3 }
| app LT app   { make_bin_op $1 Op.bin_lt   $3 }
| app GT app   { make_bin_op $1 Op.bin_gt   $3 }
| app GTE app  { make_bin_op $1 Op.bin_gte  $3 }
| app CONS app { make_bin_op $1 Op.bin_cons $3 }
| app ADD app  { make_bin_op $1 Op.bin_add  $3 }
| app SUB app  { make_bin_op $1 Op.bin_sub  $3 }
| app MUL app  { make_bin_op $1 Op.bin_mul  $3 }
| app DIV app  { make_bin_op $1 Op.bin_div  $3 }
| app MOD app  { make_bin_op $1 Op.bin_mod  $3 }
| LNOT app     { make_un_op  $1 Op.un_not   $2 }
| app DOT app  { make_bin_op $1 Op.bin_dot  $3}
| atom         { $1 }
;

atom:
  UNIT               { make_unit $1 }
| BOOL               { make_bool $1 }
| INT                { make_int $1 }
| FLOAT              { make_float $1 }
| STRING             { make_string $1 }
| LIDENT             { make_var $1 }
| LPAREN term RPAREN { $2 }
;
