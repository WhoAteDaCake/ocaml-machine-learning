
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let identity a = a