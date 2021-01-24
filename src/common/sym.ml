(* Symbolization *)

(* Symbols *)

type sym = int

module SymMap = Map.Make (struct
  type t = sym
  let compare = compare
end)

type 'a map = 'a SymMap.t

let empty = SymMap.empty
let bind = SymMap.add
let lookup = SymMap.find

(* Name-to-Symbol Tables *)

module NameMap = Map.Make (struct
  type t = string
  let compare = compare
end)

type tbl = {
  idx:  sym;
  syms: sym NameMap.t;
}

let tbl =
  { idx  = 0;
    syms = NameMap.empty }

let symbolize str tbl =
  try (NameMap.find str tbl.syms, tbl)
  with Not_found ->
    let tbl' =
      { idx  = tbl.idx + 1;
        syms = NameMap.add str tbl.idx tbl.syms }
    in
    (tbl.idx, tbl')

(* Symbol-to-Name Mapping *)

type names = string SymMap.t

let names tbl =
  let fold name sym map = SymMap.add sym name map in
  NameMap.fold fold tbl.syms SymMap.empty

let name_of = SymMap.find
