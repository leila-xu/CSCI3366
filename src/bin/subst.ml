open Typ

type binding = { tree : Typ.t
               ; var  : int
               }

type t = binding list

let fromList (bindings : (Typ.t * int) list) : t =
  List.map (fun (typ, i) -> {tree = typ; var = i}) bindings

let format subst =
  let rec repeat subst =
    match subst with
    | [] -> ""
    | [{tree; var}] ->
      Lib.fmt "%s/v%d" (Typ.format tree) var
    | {tree; var} :: subst ->
      Lib.fmt "%s/v%d, %s" (Typ.format tree) var (repeat subst)
  in
  Lib.fmt "{%s}" (repeat subst)

let rec applyToVar subst k =
  match subst with
  | [] -> Var k
  | {tree; var} :: subst ->
    (match var = k with
     | true  -> tree
     | false -> applyToVar subst k)

let rec applyToType s typ =
  match typ with
  | C -> C
  | Var k -> applyToVar s k
  | Arrow {from; too} ->
    let from = applyToType s from in
    let too  = applyToType s too
    in
    Arrow {from; too}

let compose _ _ =
  failwith "Subst.compose not impemented yet."
