(* Errors *)

exception DeclarationMismatch of Patt.t * t * t
exception ResultMismatch of t * t
exception UnboundIdentifier of Sym.sym
exception CannotApply of t
exception TooManyArgs of t * int
exception InvalidArgs of t * t
exception ConditionalBranchMismatch of t * t

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

(* Abstract Syntax Trees *)

let (builtin_idx, builtin, builtin_aenv, builtin_tenv) =
  let fold (idx, env, aenv, tenv) (id, ty) =
    let env = Type.bind idx ty env in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    (idx + 1, env, aenv, tenv)
  in
  List.fold_left fold (0, Type.env, [], []) Builtin.builtins

let rec type_of_atom env = function
  | Anf.Unit -> Type.unit
  | Anf.Bool _ -> Type.bool
  | Anf.Int _ -> Type.int
  | Anf.String _ -> Type.string
  | Anf.Blob _ -> Type.blob
  | Anf.Timestamp _ -> Type.timestamp
  | Anf.Duration _ -> Type.duration
  | Anf.Var sym -> type_of_var env sym
  | Anf.Abs (params, res, body) -> type_of_abs env params res body
  | Anf.Join (params, res, body) -> type_of_join env params res body

and type_of_var env sym =
  try Type.lookup sym env
  with Not_found -> Type.unbound_identifier sym

and type_of_abs env params res body =
  let env = Type.bind sym ty env in
  let res' = type_of_block env body in
  if Type.equal res res'
  then Type.func ty res
  else Type.declaration_mismatch sym res res'

and type_of_join env params res body =

and type_of_expr env = function
  | App (f, xs) -> type_of_app env f xs
  | Tail (f, xs) -> type_of_tail env f xs
  | Jump (j, xs) -> type_of_jump env j xs
  | Builtin (b, xs) -> type_of_builtin env b xs
  | Tuple xs -> type_of_tuple env xs
  | Proj (tuple, field) -> type_of_proj env tuple field
  | Constr (id, value) -> type_of_constr env id value
  | Atom atom -> type_of_atom env atom

and type_of_app env f xs =
  let x = type_of_atom env x in
  match type_of_atom env f with
    | Type.Fun (arg, res) ->
      if Type.equal arg x
      then res
      else Type.invalid_args arg x
    | ty -> Type.cannot_apply ty

and type_of_tail env f xs =

and type_of_jump env j xs =

and type_of_builtin env b xs =

and type_of_tuple env xs =

and type_of_proj env tuple field =

and type_of_constr env id value =

and type_of_block env = function
  | Let ((sym, ty, expr), rest) -> type_of_bind env sym ty expr rest
  | LetRec (bs, rest) -> type_of_bind_rec env bs rest
  | Case (scrut, clauses, res) -> type_of_case env scrut clauses res
  | Expr expr -> type_of_expr env expr

and type_of_bind env sym ty expr rest =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then
    let env = Type.bind sym ty env in
    type_of_block env rest
  else Type.declaration_mismatch sym ty expr

and type_of_bind_rec env bs rest =
  let fold env (sym, ty, _) = Type.bind sym ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (sym, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Type.declaration_mismatch sym ty expr
    in
    List.iter iter bs
  in
  type_of_block env rest

and type_of_case env scrut clauses res = match type_of_atom env c with
  | Type.Bool ->
    let t = type_of_block env t in
    let f = type_of_block env f in
    if Type.equal t f
    then t
    else Type.conditional_branch_mismatch t f
  | ty -> Type.invalid_condition ty

let type_of_top_bind env sym ty expr =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then Type.bind sym ty env
  else Type.declaration_mismatch sym ty expr

let type_of_top_bind_rec env bs =
  let fold env (sym, ty, _) = Type.bind sym ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (sym, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Type.declaration_mismatch sym ty expr
    in
    List.iter iter bs
  in
  env

let type_of_top env = function
  | Anf.TopLet (sym, ty, expr) -> type_of_top_bind env sym ty expr
  | Anf.TopRec bs -> type_of_top_bind_rec env bs

let type_of_file = List.fold_left type_of_top
