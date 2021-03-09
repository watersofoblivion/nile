(* Errors *)

exception DeclarationMismatch of Patt.t * t * t
exception ResultMismatch of t * t
exception UnboundIdentifier of Sym.sym
exception CannotApply of t
exception TooManyArgs of t * int
exception InvalidArgs of t * t
exception ConditionalBranchMismatch of t * t
exception InvalidUnaryOperand of Type.t * un * Type.t
exception InvalidBinaryOperands of Type.t * Type.t * bin * Type.t
exception InvalidEqualityOperands of Type.t * bin * Type.t

let declaration_mismatch sym expected actual =
  DeclarationMismatch (sym, expected, actual)
    |> raise

let result_mismatch expected actual =
  ResultMismatch (expected, actual)
    |> raise

let unbound_identifier sym =
  UnboundIdentifier sym
    |> raise

let cannot_apply ty =
  CannotApply ty
    |> raise
(*
let too_many_args ty num =
  TooManyArgs (ty, num)
    |> raise *)

let invalid_args expected actual =
  InvalidArgs (expected, actual)
    |> raise

let conditional_branch_mismatch t f =
  ConditionalBranchMismatch (t, f)
    |> raise

let invalid_unary_operand expected op actual =
  InvalidUnaryOperand (expected, op, actual)
    |> raise

let invalid_binary_operands expected actual op actual' =
  InvalidBinaryOperands (expected, actual, op, actual')
    |> raise

let invalid_equality_operands actual op actual' =
  InvalidEqualityOperands (actual, op, actual')
    |> raise

(* Patterns *)

let rec irrefutable = function
  | Patt.Var _ | Patt.Ground | Patt.Nil -> true
  | Patt.Tuple (_, patts) -> List.for_all irrefutable patts
  | Patt.Cons (hd, tl) -> irrefutable hd && irrefutable tl
  | _ -> false

(* Operators *)


let type_of_un op r = match op, r with
  | Op.Not, Type.Bool -> Type.bool
  | Op.Not, r -> invalid_unary_operand Type.bool op r

let type_of_arith l op r = match l, r with
  | Type.Int, Type.Int -> Type.int
  | l, r -> invalid_binary_operands Type.int l op r

let type_of_bool l op r = match l, r with
  | Type.Bool, Type.Bool -> Type.bool
  | l, r -> invalid_binary_operands Type.bool l op r

let type_of_eq l op r = match l, r with
  | Type.Int, Type.Int
  | Type.Bool, Type.Bool -> Type.bool
  | l, r -> invalid_equality_operands l op r

let type_of_cmp l op r = match l, r with
  | Type.Int, Type.Int -> Type.bool
  | l, r -> invalid_binary_operands Type.int l op r

let type_of_bin l op r =
  match op with
    | Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Mod -> type_of_arith l op r
    | Op.And | Op.Or -> type_of_bool l op r
    | Op.Eq | Op.Neq -> type_of_eq l op r
    | Op.Lte | Op.Lt | Op.Gt | Op.Gte -> type_of_cmp l op r
    | _ -> failwith "Dot or Cons"

(* Abstract Syntax *)

let rec type_of_expr env expr kontinue = match expr with
  | Ast.Unit -> kontinue Type.unit
  | Ast.Bool _ -> kontinue Type.bool
  | Ast.Int _ -> kontinue Type.int
  | Ast.Float _ -> kontinue Type.float
  | Ast.String _ -> kontinue Type.string
  | Ast.Blob _ -> kontinue Type.blob
  | Ast.Timestamp _ -> kontinue Type.timestamp
  | Ast.Duration _ -> kontinue Type.duration
  | Ast.Tuple exprs -> type_of_tuple env exprs kontinue
  | Ast.Constr (id, v) -> type_of_constr env id v kontinue
  | Ast.Var sym -> type_of_var env sym kontinue
  | Ast.Case (scrut, clauses) -> type_of_case env scrut clauses kontinue
  | Ast.Let (b, rest) -> type_of_bind env b rest kontinue
  | Ast.LetRec (bs, rest) -> type_of_bind_rec env bs rest kontinue
  | Ast.Proj (expr, n) -> type_of_proj env expr n kontinue
  | Ast.Abs (arity, params, res, expr) -> type_of_abs env arity params res expr kontinue
  | Ast.App (arity, f, xs) -> type_of_app env arity f xs kontinue
  | Ast.Builtin (f, xs) -> type_of_builtin env f xs kontinue

and type_of_tuple env exprs kontinue =

and type_of_constr env id v kontinue =

and type_of_var env sym kontinue =
  try
    Type.lookup sym env
      |> kontinue
  with Not_found -> Type.unbound_identifier sym

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

and type_of_proj env expr n kontinue =

and type_of_abs env arity params res expr kontinue =
  let env' = Type.bind patt ty env in
  type_of_expr env expr (fun res' ->
    if Type.equal res res'
    then
      Type.func ty res
        |> kontinue
    else Type.declaration_mismatch patt res res')

and type_of_app env arity f xs kontinue =
  type_of_expr env x (fun x ->
    match type_of_expr env f with
      | Type.Fun (arg, res) ->
        if Type.equal arg x
        then kontinue res
        else Type.invalid_args arg x
      | f -> Type.cannot_apply f)

and type_of_builtin env f xs kontinue =

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
  | Ast.TopLet b -> type_of_top_bind env b kontinue
  | Ast.TopRec bs -> type_of_top_bind_rec env bs kontinue

let rec type_of_file = List.fold_left type_of_top

let type_of_file env file = type_of_file env file (fun env -> env)
let type_of_top env top = type_of_top env top (fun env -> env)
let type_of_expr env expr = type_of_expr env expr (fun ty -> ty)
