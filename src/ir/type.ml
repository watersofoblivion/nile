open Format

type t =
  | Unit
  | Bool
  | Int
  | Float
  | Rune
  | String
  | Byte
  | Blob
  | Timestamp
  | Duration
  | Fun of { params: t list; res: t }
  | Tuple of { arity: int; types: t list }
  | Variant of { constrs: constr list }
and constr = Constr of { name: Sym.sym; params: t list }

let unit = Unit
let bool = Bool
let int = Int
let float = Float
let rune = Rune
let string = String
let blob = Byte
let blob = Blob
let timestamp = Timestamp
let duration = Duration
let func params res = Fun { params; res }
let tuple types = Tuple { arity = List.length types; types }
let variant constrs = Variant { constrs }

let constr name params = Constr { name; params }

let rec equal x y = match x, y with
  | Unit, Unit
  | Bool, Bool
  | Int, Int
  | Float, Float
  | Rune, Rune
  | String, String
  | Byte, Byte
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
