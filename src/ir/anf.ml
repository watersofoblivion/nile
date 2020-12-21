(* open Format *)
open Common

type t =
  | Bool of bool
  | Int of int
  | Var of string
  | UnOp of Op.un * t
  | BinOp of t * Op.bin * t
  | If of t * t * t
  | Let of string * Type.t * t * t
  | LetRec of (string * Type.t * t) list * t
  | Abs of string * t
  | App of t * t
(*
let rec normalize_term ast =
  normalize ast Fun.id
and normalize ast k = match ast with
  | Syntax.Ast.Bool (_, b) -> k (Bool b)
  | Syntax.Ast.Int (_, i) -> k (Int i)
  | Syntax.Ast.Var (_, id) -> k (Var id)
  | Syntax.Ast.UnOp (_, op, r) ->
    normalize r (fun r -> UnOp (op, r))
  | Syntax.Ast.BinOp (_, l, op, r) ->
    normalize l (fun l -> normalize r (fun r -> BinOp (l, op, r)))
  | Syntax.Ast.If (_, c, t, f) ->
    normalize c (fun c ->
      let t = normalize_term t in
      let f = normalize_term f in
      If (c, t, f)
    )
  | Syntax.Ast.Let (_, id, ty, expr, rest) ->
    normalize expr (fun expr ->
      let rest = normalize rest k in
      Let (id, ty, expr, rest)
    )
  | Syntax.Ast.LetRec (_, bs, rest) ->
  | Syntax.Ast.Abs (_, ps, body) ->
  | Syntax.Ast.App (_, f, xs) ->

  (* | Lambda (params, body) ->
    let body = normalize_term body in
    Lambda (params, body)
      |> k
  | Let (id, expr, body) ->
    let rest normalized =
      let body = normalize body k in
      Let (id, normalized, body)
    in
    normalize expr rest
  | If (c, t, f) ->
    let rest c =
      let t = normalize_term t in
      let f = normalize_term f in
      If (c, t, t)
        |> k
    in
    normalize_name c rest
  | App (f, xs) ->
    let rest f =
      let rest xs =
        App (f, xs)
          |> k
      in
      normalize_name xs rest
    in
    normalize_name f rest
  | ast -> k ast *)
and normalize_name ast k =
  (* let rest n =
    if is_value n
    then k n
    else
      let id = newvar () in
      Let (id, )
  in
  normalize ast rest *)
and normalize_names asts k = match asts with
  (* | [] -> k []
  | ast :: asts -> normalize_name ast (fun _ -> normalize_names asts (fun _ -> )) *) *)
