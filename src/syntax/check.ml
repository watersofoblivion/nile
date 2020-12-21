open Common

type env = (string * Type.t) list

let env = []
let bind id ty env = (id, ty) :: env
let lookup = List.assoc

exception InvalidUnaryOperand of Type.t * Op.un * Type.t
exception InvalidBinaryOperands of Type.t * Type.t * Op.bin * Type.t
exception InvalidEqualityOperands of Type.t * Op.bin * Type.t
exception DeclarationMismatch of string * Type.t * Type.t
exception ResultMismatch of Type.t * Type.t
exception UnboundIdentifier of string
exception CannotApply of Type.t
exception TooManyArgs of Type.t * int
exception InvalidArgs of Type.t * Type.t
exception InvalidCondition of Type.t
exception ConditionalBranchMismatch of Type.t * Type.t

let invalid_unary_operand expected op actual =
  InvalidUnaryOperand (expected, op, actual)
    |> raise

let invalid_binary_operands expected actual op actual' =
  InvalidBinaryOperands (expected, actual, op, actual')
    |> raise

let invalid_equality_operands actual op actual' =
  InvalidEqualityOperands (actual, op, actual')
    |> raise

let declaration_mismatch id expected actual =
  DeclarationMismatch (id, expected, actual)
    |> raise

let result_mismatch expected actual =
  ResultMismatch (expected, actual)
    |> raise

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let cannot_apply ty =
  CannotApply ty
    |> raise

let too_many_args ty num =
  TooManyArgs (ty, num)
    |> raise

let invalid_args expected actual =
  InvalidArgs (expected, actual)
    |> raise

let invalid_condition ty =
  InvalidCondition ty
    |> raise

let conditional_branch_mismatch t f =
  ConditionalBranchMismatch (t, f)
    |> raise

let type_of_un_op op r = match op, r with
  | Op.Not, Type.Bool -> Type.bool
  | Op.Not, r -> invalid_unary_operand Type.bool op r

let type_of_arith_op l op r = match l, r with
  | Type.Int, Type.Int -> Type.int
  | l, r -> invalid_binary_operands Type.int l op r

let type_of_bool_op l op r = match l, r with
  | Type.Bool, Type.Bool -> Type.bool
  | l, r -> invalid_binary_operands Type.bool l op r

let type_of_eq_op l op r = match l, r with
  | Type.Int, Type.Int
  | Type.Bool, Type.Bool -> Type.bool
  | _ -> invalid_equality_operands l op r

let type_of_cmp_op l op r = match l, r with
  | Type.Int, Type.Int -> Type.bool
  | l, r -> invalid_binary_operands Type.int l op r

let type_of_bin_op l op r =
  match op with
    | Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Mod -> type_of_arith_op l op r
    | Op.And | Op.Or -> type_of_bool_op l op r
    | Op.Eq | Op.Neq -> type_of_eq_op l op r
    | Op.Lte | Op.Lt | Op.Gt | Op.Gte -> type_of_cmp_op l op r

let rec type_of env = function
  | Ast.Bool (_, _) -> Type.bool
  | Ast.Int (_, _) -> Type.int
  | Ast.Var (_, id) ->
    begin
      try lookup id env
      with Not_found -> unbound_identifier id
    end
  | Ast.UnOp (_, op, r) ->
    let r = type_of env r in
    type_of_un_op op r
  | Ast.BinOp (_, l, op, r) ->
    let l = type_of env l in
    let r = type_of env r in
    type_of_bin_op l op r
  | Ast.If (_, c, t, f) ->
    begin
      match type_of env c with
        | Type.Bool ->
          let (t, f) = type_of env t, type_of env f in
          if Type.equal t f
          then t
          else conditional_branch_mismatch t f
        | c -> invalid_condition c
    end
  | Ast.Let (_, (_, id, ty, expr), rest) ->
    let expr' = type_of env expr in
    if Type.equal expr' ty
    then
      let env = bind id ty env in
      type_of env rest
    else declaration_mismatch id ty expr'
  | Ast.LetRec (_, vs, rest) ->
    let fold env (_, id, ty, _) = bind id ty env in
    let env = List.fold_left fold env vs in
    let _ =
      let iter (_, id, ty, expr) =
        let expr' = type_of env expr in
        if Type.equal expr' ty then () else declaration_mismatch id ty expr'
      in
      List.iter iter vs
    in
    type_of env rest
  | Ast.Abs (_, ps, res, expr) ->
    let res' =
      let fold env (_, id, ty) = bind id ty env in
      let env = List.fold_left fold env ps in
      type_of env expr
    in
    if Type.equal res res'
    then
      let fold (_, _, ty) acc = Type.func ty acc in
      List.fold_right fold ps res
    else result_mismatch res res'
  | Ast.App (_, f, xs) ->
    begin
      let ty = type_of env f in
      let rec check f xs n =
        match f, xs with
          | f, [] -> f
          | Type.Fun (a, b), x::xs ->
            let x = type_of env x in
            if Type.equal a x
            then check b xs (n + 1)
            else invalid_args a x
          | _, xs ->
            xs
              |> List.length
              |> (+) n
              |> too_many_args ty
      in
      match ty with
        | Type.Fun _ as f -> check f xs 0
        | f -> cannot_apply f
    end
