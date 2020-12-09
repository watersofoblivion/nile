open Format

type 'a t =
  | Let of 'a
  | LetRec of 'a list

let rec pp ptr top fmt = match top with
  | Let b -> fprintf fmt "let %t" (ptr b)
  | LetRec bs -> pp_bindings ptr bs fmt
and pp_bindings ptr bs fmt = match bs with
  | [] -> ()
  | b :: [] -> ptr b fmt
  | b :: bs ->
    fprintf fmt "%t and " (ptr b);
    pp_bindings ptr bs fmt
