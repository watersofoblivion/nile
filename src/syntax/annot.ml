open Common
open Ir

(* Syntax *)

type expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of Sym.sym
  | UnOp of Op.un * expr
  | BinOp of expr * Op.bin * expr
  | Case of expr * case list
  | Let of binding * expr
  | LetRec of binding list * expr
  | Abs of Patt.t * Type.t * Type.t * expr
  | App of expr * expr
and binding = Patt.t * Type.t * expr
and case = Patt.t * expr

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let unit = Unit
let bool b = Bool b
let int i = Int i
let var id = Var id
let un_op op r = UnOp (op, r)
let bin_op l op r = BinOp (l, op, r)
let case_of scrut cases = Case (scrut, cases)
let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let abs patt ty res expr = Abs (patt, ty, res, expr)
let app f xs = App (f, xs)
let binding patt ty expr = (patt, ty, expr)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops

(* Operations *)

let precedence = function
  | Unit | Bool _ | Int _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | Case _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

(* Pretty Printing *)

let rec pp_expr names expr fmt = match expr with
  | Unit -> pp_unit fmt
  | Bool b -> pp_bool b fmt
  | Int i -> pp_int i fmt
  | Var id -> pp_var names id fmt
  | UnOp (op, r) -> pp_un_op names op r fmt
  | BinOp (l, op, r) -> pp_bin_op names l op r fmt
  | Case (scrut, cases) -> pp_case_of names scrut cases fmt
  | Let (b, rest) -> pp_bind names b rest fmt
  | LetRec (bs, rest) -> pp_bind_rec names bs rest fmt
  | Abs (patt, ty, res, expr) -> pp_abs names patt ty res expr fmt
  | App (f, x) -> pp_app names f x fmt

and pp_unit fmt =
  fprintf fmt "()"

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_var names id fmt =
  Sym.name_of id names
    |> fprintf fmt "%s"

and pp_un_op names op r fmt =
  let prec = Op.un_precedence op in
  fprintf fmt "%t%t" (Op.pp_un op) (print_precedence names prec r);

and pp_bin_op names l op r fmt =
  let prec = Op.bin_precedence op in
  fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence names prec l) (Op.pp_bin op) (print_precedence names prec r)

and pp_cond names c t f fmt =
  fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp_expr names c) (pp_expr names t) (pp_expr names f)

and pp_case_of names scrut cases fmt =

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding names b) (pp_expr names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings names bs) (pp_expr names rest)

and pp_abs names patt ty res expr fmt =
  let id = Sym.name_of id names in
  fprintf fmt "(%t: %t" (Patt.pp names patt) (Type.pp ty);
  let res = pp_params names res expr fmt in
  let _ = match res with
    | Some res -> fprintf fmt "): %t" (Type.pp res)
    | None -> fprintf fmt ")"
  in
  fprintf fmt " => %t" (pp_expr names expr)

and pp_app names f x fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence names 0 f) (pp_expr names x)

and print_precedence names prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp_expr names expr)
  else fprintf fmt "%t" (pp_expr names expr)

and pp_bindings names bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding names b) in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding names (_, patt, ty, expr) fmt =
  match expr with
    | Abs (_, patt', ty, res, expr) ->
      fprintf fmt "%s(%s: %t" (Patt.pp names patt) (Patt.pp names patt') (Type.pp ty);
      let res = pp_params names res expr fmt in
      fprintf fmt ")";
      let _ = match res with
        | Some res -> fprintf fmt ": %t" (Type.pp res)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr names expr)
    | expr ->
      fprintf fmt "%t" (Patt.pp names patt);
      let _ = match ty with
        | Some ty -> fprintf fmt ": %t" (Type.pp ty)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr names expr)

and pp_params names res expr fmt = match expr with
  | Abs (_, patt, ty, res, expr) ->
    fprintf fmt ", %t: %t" (Patt.pp names patt) (Type.pp ty);
    pp_params names res expr fmt
  | _ -> res

and pp_case names patt expr fmt =

let pp_top_bind names b fmt =
  fprintf fmt "@[<hv>let %t@]" (pp_binding names b)

let pp_top_bind_rec names bs fmt =
  fprintf fmt "@[<hv>let rec %t" (pp_bindings names bs)

let pp_top names top fmt = match top with
  | TopLet (_, b) -> pp_top_bind names b fmt
  | TopRec (_, bs) -> pp_top_bind_rec names bs fmt

let pp_file names file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top names top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"

(* Annotation *)

let builtin =
  let fold env (id, ty) = Check.bind id ty env in
  List.fold_left fold Check.env Builtin.builtins

let rec annotate env = function
  | Ast.Unit _ -> annotate_unit
  | Ast.Bool (_, b) -> annotate_bool b
  | Ast.Int (_, i) -> annotate_int i
  | Ast.Var (_, id) -> annotate_var env id
  | Ast.UnOp (_, op, r) -> annotate_un_op env op r
  | Ast.BinOp (_, l, op, r) -> annotate_bin_op env l op r
  | Ast.If (_, c, t, f) -> annotate_case_of env scrut cases
  | Ast.Case (_, scrut, cases) -> annotate_case_of env scrut cases
  | Ast.Let (_, b, rest) -> annotate_bind env b rest
  | Ast.LetRec (_, bs, rest) -> annotate_bind_rec env bs rest
  | Ast.Abs (_, patt, ty, res, expr) -> annotate_abs env patt ty res expr
  | Ast.App (_, f, x) -> annotate_app env f x

and annotate_unit =
  (unit, Type.unit)

and annotate_bool b =
  (bool b, Type.bool)

and annotate_int i =
  (int i, Type.int)

and annotate_var env id =
  try (var id, Check.lookup id env)
  with Not_found -> Check.unbound_identifier id

and annotate_un_op env op r =
  let (r, r_ty) = annotate_expr env r in
  (un_op op r, Op.type_of_un op r_ty)

and annotate_bin_op env l op r =
  let (l, l_ty) = annotate_expr env l in
  let (r, r_ty) = annotate_expr env r in
  (bin_op l op r, Op.type_of_bin l_ty op r_ty)

and annotate_cond env c t f =
  let (c, ty) = annotate_expr env c in
  match ty with
    | Type.Bool ->
      let (t, t_ty) = annotate_expr env t in
      let (f, f_ty) = annotate_expr env f in
      if Type.equal t_ty f_ty
      then (cond c t f, t_ty)
      else Check.conditional_branch_mismatch t_ty f_ty
    | ty -> Check.invalid_condition ty

and annotate_bind env id ty expr rest =
  let (expr, expr_ty) = annotate_expr env expr in
  let env = match ty with
    | Some ty ->
      if Type.equal expr_ty ty
      then Check.bind id expr_ty env
      else Check.declaration_mismatch id ty expr_ty
    | None -> Check.bind id expr_ty env
  in
  let (rest, rest_ty) = annotate_expr env rest in
  let b = binding id expr_ty expr in
  (bind b rest, rest_ty)

and annotate_bind_rec env bs rest =
  let fold env (_, id, ty, _) = match ty with
    | Some ty -> Check.bind id ty env
    | None -> Check.annotation_required id
  in
  let env = List.fold_left fold env bs in
  let bs =
    let map (loc, id, ty, expr) =
      let (expr, expr_ty) = annotate_expr env expr in
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then binding id expr_ty expr
          else Check.declaration_mismatch id ty expr_ty
        | None -> Check.annotation_required id
    in
    List.map map bs
  in
  let (rest, rest_ty) = annotate_expr env rest in
  (bind_rec bs rest, rest_ty)

and annotate_abs env id ty res expr =
  let (expr, expr_ty) =
    let env = Check.bind id ty env in
    annotate_expr env expr
  in
  let res = match res with
    | Some res ->
      if Type.equal res expr_ty
      then expr_ty
      else Check.result_mismatch res expr_ty
    | None -> expr_ty
  in
  (abs id ty res expr, Type.func ty res)

and annotate_app env f x =
  let (f, f_ty) = annotate_expr env f in
  let (x, res) = match f_ty with
    | Type.Fun (arg, res) ->
        let (x, x_ty) = annotate_expr env x in
        if Type.equal arg x_ty
        then (x, res)
        else Check.invalid_args arg x_ty
    | _ -> Check.cannot_apply f_ty
  in
  (app f x, res)

let annotate_top_bind env id ty expr =
  let (expr, expr_ty) = annotate_expr env expr in
  let env = match ty with
    | Some ty ->
      if Type.equal expr_ty ty
      then Check.bind id expr_ty env
      else Check.declaration_mismatch id ty expr_ty
    | None -> Check.bind id expr_ty env
  in
  let b = binding id expr_ty expr in
  (top_bind b, env)

let annotate_top_bind_rec env bs =
  let fold env (_, id, ty, _) = match ty with
    | Some ty -> Check.bind id ty env
    | None -> Check.annotation_required id
  in
  let env = List.fold_left fold env bs in
  let bs =
    let map (loc, id, ty, expr) =
      let (expr, expr_ty) = annotate_expr env expr in
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then binding id expr_ty expr
          else Check.declaration_mismatch id ty expr_ty
        | None -> Check.annotation_required id
    in
    List.map map bs
  in
  (top_bind_rec bs, env)

let annotate_top env = function
  | Ast.TopLet (_, (_, id, ty, expr)) -> annotate_top_bind env id ty expr
  | Ast.TopRec (_, bs) -> annotate_top_bind_rec env bs

let rec annotate_file env = function
  | [] -> []
  | top :: file ->
    let (top, env) = annotate_top env top in
    let file = annotate_file env file in
    top :: file
