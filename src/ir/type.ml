open Format

type t =
  | Unit
  | Bool
  | Int
  | Float
  | String
  | Blob
  | Timestamp
  | Duration
  | Fun of t list * t
  | Tuple of int * t list
  | Variant of constr list
and constr = Sym.sym * t option

let unit = Unit
let bool = Bool
let int = Int
let float = Float
let string = String
let blob = Blob
let timestamp = Timestamp
let duration = Duration
let func args ret = Fun (args, ret)
let tuple tys = Tuple (List.length tys, tys)
let variant constrs = Variant constrs

let constr id ty = (id, ty)

let rec equal x y = match x, y with
  | Unit, Unit
  | Bool, Bool
  | Int, Int
  | Float, Float
  | String, String
  | Blob, Blob
  | Timestamp, Timestamp
  | Duration, Duration -> true
  | Fun (args, ret), Fun (args', ret') ->
    List.for_all2 equal args args'
      && equal ret ret'
  | Tuple (len, tys), Tuple (len', tys') ->
    len = len' && List.for_all2 equal tys tys'
  | Variant constrs, Variant constrs' ->
    let match_constr constr =
      try
        let (_, ty') = List.assoc id constrs' in
        match ty, ty' with
          | None, None -> true
          | Some ty, Some ty' -> equal ty ty'
          | _ -> false
      with Not_found -> false
    in
    List.for_all match_constr constrs
  | _ -> false

module IdMap = Map.Make (struct
  type t = Sym.sym
  let compare = compare
end)

type env = t Sym.map

let env = Sym.empty
let bind patt ty env = match patt with
  | Patt.Var sym -> Sym.bind sym ty env
  | _ -> env
let lookup = Sym.lookup

let of_pattern patt ty = match patt, ty with
  | Patt.Unit, Unit
  | Patt.Int _, Int
  | Patt.Bool _, Bool
  | Patt.Float _, Float
  | Patt.String _, String
  | Patt.Var _, _
  | Patt.Ground, _ -> true
  | _ -> false
