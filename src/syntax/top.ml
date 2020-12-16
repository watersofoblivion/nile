open Format

module type Lang =
  sig
    type t
    type p = Loc.t * string * Type.t

    val is_abs : t -> (p list * Type.t * t) option
    val pp : t -> formatter -> unit
  end

module type Top =
  sig
    type e
    type b = Loc.t * string * Type.t * e
    type t = private
      | Let of Loc.t * b
      | LetRec of Loc.t * b list

    val bind : Loc.t -> b -> t
    val bind_rec : Loc.t -> b list -> t
    val pp : t -> formatter -> unit
  end

module Make = functor (Lang: Lang) ->
  struct
    type e = Lang.t
    type b = Loc.t * string * Type.t * e

    type t =
      | Let of Loc.t * b
      | LetRec of Loc.t * b list

    let bind loc b = Let (loc, b)
    let bind_rec loc bs = LetRec (loc, bs)

    let pp top _ = match top with
      | Let _ -> ()
      | LetRec _ -> ()
  end

module Ast: Top with type e = Ast.t = Make (struct
  type t = Ast.t
  type p = Loc.t * string * Type.t
  let is_abs = function
    | Ast.Abs (_, ps, ty, expr) -> Some (ps, ty, expr)
    | _ -> None
  let pp = Ast.pp
end)
