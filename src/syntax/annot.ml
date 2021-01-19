open Format
open Common

(* Syntax *)

type expr =
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Var of Loc.t * string
  | UnOp of Loc.t * Op.un * expr
  | BinOp of Loc.t * expr * Op.bin * expr
  | If of Loc.t * expr * expr * expr
  | Let of Loc.t * binding * expr
  | LetRec of Loc.t * binding list * expr
  | Abs of Loc.t * string * Type.t * Type.t * expr
  | App of Loc.t * expr * expr
and binding = Loc.t * string * Type.t * expr

type top =
  | TopLet of Loc.t * binding
  | TopRec of Loc.t * binding list

type file = top list

(* Constructors *)

let bool loc b = Bool (loc, b)
let int loc i = Int (loc, i)
let var loc id = Var (loc, id)
let un_op loc op r = UnOp (loc, op, r)
let bin_op loc l op r = BinOp (loc, l, op, r)
let bind loc b rest = Let (loc, b, rest)
let bind_rec loc bs rest = LetRec (loc, bs, rest)
let abs loc id ty res expr = Abs (loc, id, ty, res, expr)
let app loc f xs = App (loc, f, xs)
let cond loc c t f = If (loc, c, t, f)
let binding loc id ty expr = (loc, id, ty, expr)

let top_bind loc b = TopLet (loc, b)
let top_bind_rec loc bs = TopRec (loc, bs)

let file tops = tops

(* Annotation *)

module Checker = Check.Make (struct
  type t = string
  let compare = compare
end)

type env = Checker.env
let env = Checker.env
let builtin =
  let fold env (id, ty) = Checker.bind id ty env in
  List.fold_left fold env Builtin.builtins

let rec annotate_expr env = function
  | Unannot.Bool (loc, b) -> annotate_bool loc b
  | Unannot.Int (loc, i) -> annotate_int loc i
  | Unannot.Var (loc, id) -> annotate_var env loc id
  | Unannot.UnOp (loc, op, r) -> annotate_un_op env loc op r
  | Unannot.BinOp (loc, l, op, r) -> annotate_bin_op env loc l op r
  | Unannot.If (loc, c, t, f) -> annotate_cond env loc c t f
  | Unannot.Let (loc, (loc', id, ty, expr), rest) -> annotate_bind env loc loc' id ty expr rest
  | Unannot.LetRec (loc, bs, rest) -> annotate_bind_rec env loc bs rest
  | Unannot.Abs (loc, id, ty, res, expr) -> annotate_abs env loc id ty res expr
  | Unannot.App (loc, f, x) -> annotate_app env loc f x

and annotate_bool loc b =
  (bool loc b, Type.bool)

and annotate_int loc i =
  (int loc i, Type.int)

and annotate_var env loc id =
  try (var loc id, Checker.lookup id env)
  with Not_found -> Checker.unbound_identifier id

and annotate_un_op env loc op r =
  let (r, r_ty) = annotate_expr env r in
  (un_op loc op r, Op.type_of_un op r_ty)

and annotate_bin_op env loc l op r =
  let (l, l_ty) = annotate_expr env l in
  let (r, r_ty) = annotate_expr env r in
  (bin_op loc l op r, Op.type_of_bin l_ty op r_ty)

and annotate_cond env loc c t f =
  let (c, ty) = annotate_expr env c in
  match ty with
    | Type.Bool ->
      let (t, t_ty) = annotate_expr env t in
      let (f, f_ty) = annotate_expr env f in
      if Type.equal t_ty f_ty
      then (cond loc c t f, t_ty)
      else Checker.conditional_branch_mismatch t_ty f_ty
    | ty -> Checker.invalid_condition ty

and annotate_bind env loc loc' id ty expr rest =
  let (expr, expr_ty) = annotate_expr env expr in
  let env = match ty with
    | Some ty ->
      if Type.equal expr_ty ty
      then Checker.bind id expr_ty env
      else Checker.declaration_mismatch id ty expr_ty
    | None -> Checker.bind id expr_ty env
  in
  let (rest, rest_ty) = annotate_expr env rest in
  let b = binding loc' id expr_ty expr in
  (bind loc b rest, rest_ty)

and annotate_bind_rec env loc bs rest =
  let fold env (_, id, ty, _) = match ty with
    | Some ty -> Checker.bind id ty env
    | None -> Checker.annotation_required id
  in
  let env = List.fold_left fold env bs in
  let bs =
    let map (loc, id, ty, expr) =
      let (expr, expr_ty) = annotate_expr env expr in
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then binding loc id expr_ty expr
          else Checker.declaration_mismatch id ty expr_ty
        | None -> Checker.annotation_required id
    in
    List.map map bs
  in
  let (rest, rest_ty) = annotate_expr env rest in
  (bind_rec loc bs rest, rest_ty)

and annotate_abs env loc id ty res expr =
  let (expr, expr_ty) =
    let env = Checker.bind id ty env in
    annotate_expr env expr
  in
  let res = match res with
    | Some res ->
      if Type.equal res expr_ty
      then expr_ty
      else Checker.result_mismatch res expr_ty
    | None -> expr_ty
  in
  (abs loc id ty res expr, Type.func ty res)

and annotate_app env loc f x =
  let (f, f_ty) = annotate_expr env f in
  let (x, res) = match f_ty with
    | Type.Fun (arg, res) ->
        let (x, x_ty) = annotate_expr env x in
        if Type.equal arg x_ty
        then (x, res)
        else Checker.invalid_args arg x_ty
    | _ -> Checker.cannot_apply f_ty
  in
  (app loc f x, res)

let annotate_top_bind env loc loc' id ty expr =
  let (expr, expr_ty) = annotate_expr env expr in
  let env = match ty with
    | Some ty ->
      if Type.equal expr_ty ty
      then Checker.bind id expr_ty env
      else Checker.declaration_mismatch id ty expr_ty
    | None -> Checker.bind id expr_ty env
  in
  let b = binding loc' id expr_ty expr in
  (top_bind loc b, env)

let annotate_top_bind_rec env loc bs =
  let fold env (_, id, ty, _) = match ty with
    | Some ty -> Checker.bind id ty env
    | None -> Checker.annotation_required id
  in
  let env = List.fold_left fold env bs in
  let bs =
    let map (loc, id, ty, expr) =
      let (expr, expr_ty) = annotate_expr env expr in
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then binding loc id expr_ty expr
          else Checker.declaration_mismatch id ty expr_ty
        | None -> Checker.annotation_required id
    in
    List.map map bs
  in
  (top_bind_rec loc bs, env)

let annotate_top env = function
  | Unannot.TopLet (loc, (loc', id, ty, expr)) -> annotate_top_bind env loc loc' id ty expr
  | Unannot.TopRec (loc, bs) -> annotate_top_bind_rec env loc bs

let rec annotate_file env = function
  | [] -> []
  | top :: file ->
    let (top, env) = annotate_top env top in
    let file = annotate_file env file in
    top :: file

(* Operations *)

let precedence = function
  | Bool _ | Int _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let loc_expr = function
  | Bool (loc, _)
  | Int (loc, _)
  | UnOp (loc, _, _)
  | BinOp (loc, _, _, _)
  | Let (loc, _, _)
  | LetRec (loc, _, _)
  | Abs (loc, _, _, _, _)
  | App (loc, _, _)
  | Var (loc, _)
  | If (loc, _, _, _) -> loc

let loc_top = function
  | TopLet (loc, _)
  | TopRec (loc, _) -> loc

(* Pretty Printing *)

let rec pp_expr expr fmt = match expr with
  | Bool (_, b) -> pp_bool b fmt
  | Int (_, i) -> pp_int i fmt
  | Var (_, id) -> pp_var id fmt
  | UnOp (_, op, r) -> pp_un_op op r fmt
  | BinOp (_, l, op, r) -> pp_bin_op l op r fmt
  | Let (_, b, rest) -> pp_bind b rest fmt
  | LetRec (_, bs, rest) -> pp_bind_rec bs rest fmt
  | Abs (_, id, ty, res, expr) -> pp_abs id ty res expr fmt
  | App (_, f, x) -> pp_app f x fmt
  | If (_, c, t, f) -> pp_cond c t f fmt

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_var id fmt =
  fprintf fmt "%s" id

and pp_un_op op r fmt =
  let prec = Op.un_precedence op in
  fprintf fmt "%t%t" (Op.pp_un op) (print_precedence prec r);

and pp_bin_op l op r fmt =
  let prec = Op.bin_precedence op in
  fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence prec l) (Op.pp_bin op) (print_precedence prec r)

and pp_bind b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding b) (pp_expr rest)

and pp_bind_rec bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings bs) (pp_expr rest)

and pp_abs id ty res expr fmt =
  fprintf fmt "(%s: %t" id (Type.pp ty);
  let res = pp_params res expr fmt in
  fprintf fmt "): %t => %t" (Type.pp res) (pp_expr expr)

and pp_app f x fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence 0 f) (pp_expr x)

and pp_cond c t f fmt =
  fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp_expr c) (pp_expr t) (pp_expr f)

and print_precedence prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp_expr expr)
  else fprintf fmt "%t" (pp_expr expr)

and pp_bindings bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding b) in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding (_, id, ty, expr) fmt = match expr with
  | Abs (_, id', ty, res, expr) ->
    fprintf fmt "%s(%s: %t" id id' (Type.pp ty);
    let res = pp_params res expr fmt in
    fprintf fmt "): %t =@;<1 2>%t" (Type.pp res) (pp_expr expr)
  | expr -> fprintf fmt "%s: %t =@;<1 2>%t" id (Type.pp ty) (pp_expr expr)

and pp_params res expr fmt = match expr with
  | Abs (_, id, ty, res, expr) ->
    fprintf fmt ", %s: %t" id (Type.pp ty);
    pp_params res expr fmt
  | _ -> res

let pp_top_bind b fmt =
  fprintf fmt "@[<hv>let %t@]" (pp_binding b)

let pp_top_bind_rec bs fmt =
  fprintf fmt "@[<hv>let rec %t" (pp_bindings bs)

let pp_top top fmt = match top with
  | TopLet (_, b) -> pp_top_bind b fmt
  | TopRec (_, bs) -> pp_top_bind_rec bs fmt

let pp_file file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"

(* Type Checking *)

let rec type_of_expr env = function
  | Bool _ -> type_of_bool
  | Int _ -> type_of_int
  | Var (_, id) -> type_of_var env id
  | UnOp (_, op, r) -> type_of_un_op env op r
  | BinOp (_, l, op, r) -> type_of_bin_op env l op r
  | If (_, c, t, f) -> type_of_cond env c t f
  | Let (_, (_, id, ty, expr), rest) -> type_of_bind env id ty expr rest
  | LetRec (_, bs, rest) -> type_of_bind_rec env bs rest
  | Abs (_, id, ty, res, body) -> type_of_abs env id ty res body
  | App (_, f, x) -> type_of_app env f x

and type_of_bool = Type.bool
and type_of_int = Type.int

and type_of_var env id =
  try Checker.lookup id env
  with Not_found -> Checker.unbound_identifier id

and type_of_un_op env op r =
  let r = type_of_expr env r in
  Op.type_of_un op r

and type_of_bin_op env l op r =
  let l = type_of_expr env l in
  let r = type_of_expr env r in
  Op.type_of_bin l op r

and type_of_cond env c t f =
  match type_of_expr env c with
    | Type.Bool ->
      let (t, f) = (type_of_expr env t, type_of_expr env f) in
      if Type.equal t f
      then t
      else Checker.conditional_branch_mismatch t f
    | ty -> Checker.invalid_condition ty

and type_of_bind env id ty expr rest =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then
    let env = Checker.bind id ty env in
    type_of_expr env rest
  else Checker.declaration_mismatch id ty expr

and type_of_bind_rec env bs rest =
  let fold env (_, id, ty, _) = Checker.bind id ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (_, id, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Checker.declaration_mismatch id ty expr
    in
    List.iter iter bs
  in
  type_of_expr env rest

and type_of_abs env id ty res body =
  let env' = Checker.bind id ty env in
  let res' = type_of_expr env' body in
  if Type.equal res res'
  then Type.func ty res
  else Checker.result_mismatch res res'

and type_of_app env f x =
  match type_of_expr env f with
    | Type.Fun (arg, res) ->
      let x = type_of_expr env x in
      if Type.equal arg x
      then res
      else Checker.invalid_args arg x
    | ty -> Checker.cannot_apply ty

let type_of_top_bind env id ty expr =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then Checker.bind id ty env
  else Checker.declaration_mismatch id ty expr

let type_of_top_bind_rec env bs =
  let fold env (_, id, ty, _) = Checker.bind id ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (_, id, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Checker.declaration_mismatch id ty expr
    in
    List.iter iter bs
  in
  env

let type_of_top env = function
  | TopLet (_, (_, id, ty, expr)) -> type_of_top_bind env id ty expr
  | TopRec (_, bs) -> type_of_top_bind_rec env bs

let type_of_file = List.fold_left type_of_top
