open Format
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
  | Case of expr * clause list * Type.t
  | Let of binding * expr
  | LetRec of binding list * expr
  | Abs of Patt.t * Type.t * Type.t * expr
  | App of expr * expr
and binding = Patt.t * Type.t * expr
and clause = Patt.t * expr

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let unit = Unit
let bool b = Bool b
let int i = Int i
let var sym = Var sym
let un_op op r = UnOp (op, r)
let bin_op l op r = BinOp (l, op, r)
let case scrut clauses res = Case (scrut, clauses, res)
let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let abs patt ty res expr = Abs (patt, ty, res, expr)
let app f xs = App (f, xs)
let binding patt ty expr = (patt, ty, expr)
let clause patt expr = (patt, expr)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops

(* Annotation *)

(* let builtin =
  let fold env (id, ty) = Type.bind id ty env in
  List.fold_left fold Type.env Builtin.builtins *)

let bindings_env =
  let fold env (_, patt, ty, _) = match ty with
    | Some ty -> Type.bind patt ty env
    | None -> Type.annotation_required patt
  in
  List.fold_left fold

let rec annotate_expr env expr kontinue = match expr with
  | Ast.Unit _ -> annotate_unit kontinue
  | Ast.Bool (_, b) -> annotate_bool b kontinue
  | Ast.Int (_, i) -> annotate_int i kontinue
  | Ast.Var (_, sym) -> annotate_var env sym kontinue
  | Ast.UnOp (_, op, r) -> annotate_un_op env op r kontinue
  | Ast.BinOp (_, l, op, r) -> annotate_bin_op env l op r kontinue
  | Ast.If (_, c, t, f) -> annotate_cond env c t f kontinue
  | Ast.Case (_, scrut, clauses) -> annotate_case env scrut clauses kontinue
  | Ast.Let (_, (_, patt, ty, expr), rest) -> annotate_bind env patt ty expr rest kontinue
  | Ast.LetRec (_, bs, rest) -> annotate_bind_rec env bs rest kontinue
  | Ast.Abs (_, patt, ty, res, expr) -> annotate_abs env patt ty res expr kontinue
  | Ast.App (_, f, x) -> annotate_app env f x kontinue

and annotate_unit kontinue =
  unit
    |> kontinue Type.unit

and annotate_bool b kontinue =
  bool b
    |> kontinue Type.bool

and annotate_int i kontinue =
  int i
    |> kontinue Type.int

and annotate_var env sym kontinue =
  try
    let ty = Type.lookup sym env in
    var sym
      |> kontinue ty
  with Not_found -> Type.unbound_identifier sym

and annotate_un_op env op r kontinue =
  annotate_expr env r (fun ty r ->
    let ty = Op.type_of_un op ty in
    un_op op r
      |> kontinue ty)

and annotate_bin_op env l op r kontinue =
  annotate_expr env l (fun l_ty l ->
    annotate_expr env r (fun r_ty r ->
      let (ty, expr) = match op with
        | Op.And ->
          case l [
            bool false
              |> clause (Patt.bool false);
            clause Patt.ground r]
          Type.bool
        | Op.Or ->
          case l [
            bool true
              |> clause (Patt.bool true);
            clause Patt.ground r]
          ] Type.bool
        | op ->
          bin_op l op r
      in
      let ty = Op.type_of_bin l_ty op r_ty in
      kontinue ty expr))

and annotate_cond env c t f kontinue =
  annotate_expr env c (fun c_ty c ->
    match c_ty with
      | Type.Bool ->
        annotate_expr env t (fun t_ty t ->
          annotate_expr env f (fun f_ty f ->
            if Type.equal t_ty f_ty
            then
              case c [
                clause (Patt.bool true)  t;
                clause (Patt.bool false) f]
                t_ty
                |> kontinue t_ty
            else Type.conditional_branch_mismatch t_ty f_ty))
      | ty -> Type.invalid_condition ty)

(* and annotate_case env scrut clauses kontinue = *)
and annotate_case _ _ _ _ = failwith "TODO"

and annotate_bind env patt ty expr rest kontinue =
  annotate_binding env patt ty expr (fun env b ->
    annotate_expr env rest (fun rest_ty rest ->
      bind b rest
        |> kontinue rest_ty))

and annotate_binding env patt ty expr kontinue =
  annotate_expr env expr (fun expr_ty expr ->
    let env = match ty with
      | Some ty ->
        if Type.equal expr_ty ty
        then Type.bind patt expr_ty env
        else Type.declaration_mismatch patt ty expr_ty
      | None -> Type.bind patt expr_ty env
    in
    binding patt expr_ty expr
      |> kontinue env)

and annotate_bind_rec env bs rest kontinue =
  let env = bindings_env env bs in
  annotate_bindings env bs (fun bs ->
    annotate_expr env rest (fun rest_ty rest ->
      bind_rec bs rest
        |> kontinue rest_ty))

and annotate_bindings env bs kontinue = match bs with
  | [] -> kontinue []
  | (_, patt, ty, expr) :: bs ->
    annotate_expr env expr (fun expr_ty expr ->
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then
            annotate_bindings env bs (fun bs ->
              binding patt expr_ty expr :: bs
                |> kontinue)
          else Type.declaration_mismatch patt ty expr_ty
        | None -> Type.annotation_required patt)

and annotate_abs env patt ty res expr kontinue =
  let env = Type.bind patt ty env in
  annotate_expr env expr (fun expr_ty expr ->
    let res = match res with
      | Some res ->
        if Type.equal res expr_ty
        then expr_ty
        else Type.result_mismatch res expr_ty
      | None -> expr_ty
    in
    abs patt ty res expr
      |> kontinue (Type.func ty res))

and annotate_app env f x kontinue =
  annotate_expr env f (fun f_ty f ->
    match f_ty with
      | Type.Fun (arg, res) ->
          annotate_expr env x (fun x_ty x ->
            if Type.equal arg x_ty
            then
              app f x
                |> kontinue res
            else Type.invalid_args arg x_ty)
      | _ -> Type.cannot_apply f_ty)

let annotate_top_bind env patt ty expr kontinue =
  annotate_binding env patt ty expr (fun env b ->
    top_bind b
      |> kontinue env)

let annotate_top_bind_rec env bs kontinue =
  let env = bindings_env env bs in
  annotate_bindings env bs (fun bs ->
    top_bind_rec bs
      |> kontinue env)

let annotate_top env top kontinue = match top with
  | Ast.TopLet (_, (_, patt, ty, expr)) -> annotate_top_bind env patt ty expr kontinue
  | Ast.TopRec (_, bs) -> annotate_top_bind_rec env bs kontinue

let rec annotate_file env file kontinue = match file with
  | [] -> []
  | top :: file ->
    annotate_top env top (fun env top ->
      annotate_file env file (fun env file ->
        top :: file
          |> kontinue env))

let annotate_file env file = annotate_file env file (fun env file -> (env, file))
let annotate_top env top = annotate_top env top (fun env top -> (env, top))
let annotate_expr env expr = annotate_expr env expr (fun _ expr -> expr)

(* Operations *)

let precedence = function
  | Unit | Bool _ | Int _ | Var _ -> 0
  | App _ -> 1
  | UnOp (op, _) -> Op.un_precedence op
  | BinOp (_, op, _) -> Op.bin_precedence op
  | Case _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

(* Pretty Printing *)

let rec pp_expr names expr fmt = match expr with
  | Unit -> pp_unit fmt
  | Bool b -> pp_bool b fmt
  | Int i -> pp_int i fmt
  | Var sym -> pp_var names sym fmt
  | UnOp (op, r) -> pp_un_op names op r fmt
  | BinOp (l, op, r) -> pp_bin_op names l op r fmt
  | Case (scrut, clauses, _) -> pp_case names scrut clauses fmt
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

and pp_var names sym fmt =
  Sym.name_of sym names
    |> fprintf fmt "%s"

and pp_un_op names op r fmt =
  let prec = Op.un_precedence op in
  fprintf fmt "%t%t" (Op.pp_un op) (print_precedence names prec r);

and pp_bin_op names l op r fmt =
  let prec = Op.bin_precedence op in
  fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence names prec l) (Op.pp_bin op) (print_precedence names prec r)

(* and pp_case names scrut clauses fmt = *)
and pp_case _ _ _ _ = failwith "TODO"

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding names b) (pp_expr names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings names bs) (pp_expr names rest)

and pp_abs names patt ty res expr fmt =
  let id = Sym.name_of id names in
  fprintf fmt "(%t: %t" (Patt.pp names patt) (Type.pp ty);
  let res = pp_params names res expr fmt in
  fprintf fmt "): %t => %t" (Type.pp res) (pp_expr names expr)

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
      fprintf fmt "): %t =@;<1 2>%t" (Type.pp res) (pp_expr names expr)
    | expr ->
      fprintf fmt "%t: %t =@;<1 2>%t" (Patt.pp names patt) (Type.pp ty) (pp_expr names expr)

and pp_params names res expr fmt = match expr with
  | Abs (_, patt, ty, res, expr) ->
    fprintf fmt ", %t: %t" (Patt.pp names patt) (Type.pp ty);
    pp_params names res expr fmt
  | _ -> res

(* and pp_clause names patt expr fmt = *)
and pp_clause _ _ _ _ = failwith "TODO"

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

(* Type Checking *)

let rec type_of_expr env expr kontinue = match expr with
  | Unit -> kontinue Type.unit
  | Bool _ -> kontinue Type.bool
  | Int _ -> kontinue Type.int
  | Var sym -> type_of_var env sym kontinue
  | UnOp (op, r) -> type_of_un_op env op r kontinue
  | BinOp (l, op, r) -> type_of_bin_op env l op r kontinue
  | Case (scrut, clauses) -> type_of_case env scrut clauses kontinue
  | Let (b, rest) -> type_of_bind env b rest kontinue
  | LetRec (bs, rest) -> type_of_bind_rec env bs rest kontinue
  | Abs (patt, ty, res, expr) -> type_of_abs env patt ty res expr kontinue
  | App (f, x) -> type_of_app env f x kontinue

and type_of_var env sym kontinue =
  try
    Type.lookup sym env
      |> kontinue
  with Not_found -> Type.unbound_identifier sym

and type_of_un_op env op r kontinue =
  type_of_expr env r
    |> Op.type_of_un op
    |> kontinue

and type_of_bin_op env l op r kontinue =
  let l = type_of_expr env l in
  type_of_expr env r
    |> Op.type_of_bin l op
    |> kontinue

and type_of_case env scrut clauses kontinue =
  let scrut = type_of_expr env scrut in
  let _ =
    let iter (patt, _) =
      if Type.of_pattern patt scrut
      then ()
      else failwith "Invalid pattern"
    in
    List.iter iter clauses
  in
  let map (patt, expr) =
    let env = Type.bind patt scrut env in
    type_of_expr env expr
  in
  let fold expected actual =
    if Type.equal expected actual
    then ()
    else failwith "Mismatched clause types"
  in
  match List.map map clauses with
    | [] -> failwith "No clauses"
    | hd::tl ->
      List.iter fold hd tl
        |> kontinue

and type_of_bind env (patt, ty, expr) rest kontinue =
  type_of_expr env expr (fun expr ->
    if Type.equal ty expr
    then
      let env = Type.bind patt ty env in
      type_of_expr env rest kontinue
    else Type.declaration_mismatch patt ty expr)

and type_of_bind_rec env bs rest kontinue =
  let fold env (patt, ty, _) = Type.bind patt ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (patt, ty, expr) =
      let ty' = type_of_expr expr env in
      if Type.equal ty ty'
      then ()
      else Type.declaration_mismatch patt ty expr
    in
    List.iter iter bs
  in
  type_of_expr env rest

and type_of_abs env patt ty res expr kontinue =
  let env' = Type.bind patt ty env in
  type_of_expr env expr (fun res' ->
    if Type.equal res res'
    then
      Type.func ty res
        |> kontinue
    else Type.declaration_mismatch patt res res')

and type_of_app env f x kontinue =
  type_of_expr env x (fun x ->
    match type_of_expr env f with
      | Type.Fun (arg, res) ->
        if Type.equal arg x
        then kontinue res
        else Type.invalid_args arg x
      | f -> Type.cannot_apply f)

let type_of_top_bind env (patt, ty, expr) kontinue =
  type_of_expr env expr (fun expr ->
    if Type.equal ty expr
    then
      Type.bind patt ty env
        |> kontinue
    else Type.declaration_mismatch patt ty expr)

let type_of_top_bind_rec env bs =
  let fold env (patt, ty, _) = Type.bind patt ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (patt, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Type.declaration_mismatch patt ty expr
    in
    List.iter iter bs
  in
  env

let type_of_top env top kontinue = match top with
  | TopLet b -> type_of_top_bind env b kontinue
  | TopRec bs -> type_of_top_bind_rec env bs kontinue

let rec type_of_file = List.fold_left type_of_top

let type_of_file env file = type_of_file env file (fun env -> env)
let type_of_top env top = type_of_top env top (fun env -> env)
let type_of_expr env expr = type_of_expr env expr (fun ty -> ty)
