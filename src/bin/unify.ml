
(* disagreement : Typ.t list -> Typ.t list
*)

let checkFroms head x =
  match (head) with
  | Typ.Arrow a ->  (match (x) with
      | Typ. Arrow b -> ( match a.from = b.from with
          | true -> true
          | false -> false
        )
      | _ -> false
    )
  | _ -> false;;

  let checkToos head x =
    match (head) with
    | Typ.Arrow a ->  (match (x) with
        | Typ. Arrow b -> ( match a.too = b.too with
            | true -> true
            | false -> false
          )
        | _ -> false
      )
    | _ -> false;;

let checkArrow head x =
  match (head) with
  | Typ.Arrow _ -> ( match (x) with
      | Typ. Arrow _ -> true
      | _ -> false
    )
  | _ -> false;;

let getSubTree x =
  match x with
  | Typ.Arrow a -> a.from
  | _ -> failwith "Error";;

let getSubTreeToo x =
  match x with
  | Typ.Arrow a -> a.too
  | _ -> failwith "Error";;

let disagreement treelist =
  let head = List.hd treelist in
  match (List.for_all (fun x -> checkArrow head x) (List.tl treelist)) with
  | true ->
    (match (List.for_all (fun x -> checkFroms head x) (List.tl treelist)) with
     | true -> (match (List.for_all (fun x -> checkToos head x) (List.tl treelist)) with
         | true -> []
           | false -> List.map getSubTreeToo treelist ) (*continue searching the checkToos now *)
     | false -> List.map getSubTree treelist (*return the from subtree*)
    )
  | false -> [];;

  (* unify : Typ.t list -> Subst.t option

      This is a stub.
                  *)
let unify _ = None
