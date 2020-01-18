open Modules

module Map = IntMap

let string_of_index = string_of_int

let empty x y default =
	let rec fill mk map = function
	| -1 -> map
	| n ->  fill mk (Map.add n (mk ()) map) (n - 1)
	in
	fill (fun () -> fill (fun () -> default) Map.empty x) Map.empty y

let row_to_str ?show_index:(show_index=false) row f =
	Map.fold (fun i v txt ->
		let pf  = if show_index then (string_of_index i) ^ ", " else "" in
		txt ^ Printf.sprintf "(%s%s), " pf (f v)
	) row ""
	|> Helpers.rm_last_char
	|> Helpers.rm_last_char

let to_string ?show_index:(show_index=false) matrix f =
	Map.fold (fun i row txt ->
		let pf = if show_index then (string_of_index i) ^ ":" else "" in
		txt ^ Printf.sprintf "%s [%s]\n" pf (row_to_str ~show_index:show_index row f)
	) matrix ""
	|> Helpers.rm_last_char