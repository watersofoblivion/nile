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
  | Record of { fields: field list }
  | Variant of { constrs: constr list }
  | Named of { name: Sym.sym }
and field = Field of { name: Sym.sym; ty: t }
and constr = Constr of { name: Sym.sym; params: t list }

let unit = Unit
let bool = Bool
let int = Int
let float = Float
let rune = Rune
let string = String
let byte = Byte
let blob = Blob
let timestamp = Timestamp
let duration = Duration
let func params ret = Fun { params; ret }
let tuple types = Tuple { arity = List.length types; types }
let record fields = Record { fields }
let variant constrs = Variant { constrs }
let named name = Named { name }

let field name ty = Field { name; ty }
let constr name params = Constr { name; params }

let rec equal x y = match x, y with
  | Unit, Unit
  | Bool, Bool
  | Int, Int
  | Float, Float
  | Rune, Rune
  | String, String
  | Blob, Blob
  | Timestamp, Timestamp
  | Duration, Duration -> true
  | Fun (args, ret), Fun (args', ret') ->
    List.for_all2 equal args args'
      && equal ret ret'
  | Tuple (len, tys), Tuple (len', tys') ->
    len = len' && List.for_all2 equal tys tys'
  | Record fields, Record fields' ->
    let match_field (id, ty) =
      try
        let (_, ty') = List.assoc id fields' in
        equal ty ty'
      with Not_found -> false
    in
    List.for_all match_field fields
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
