open Modules

type 'a t = ('a CCArray.t) CCArray.t

let make = CCArray.make_matrix

let set mt x y value =
	let row = mt.(y) in
	let _ = row.(x) <- value in
	let _ = mt.(y) <- row in
	mt

let get mt x y =
	let row = mt.(y) in
	row.(x)

let size = CCArray.length

let fold_x mt f acc y_idx =
 	let size = CCArray.length mt in
	let rec loop x acc =
		if x = size then
			acc
		else
			loop (x + 1) (f x y_idx acc)
	in
	loop 0 acc

(*
	Assumes matrix is the same length
	And is non empty 
*)
let fold mt f acc =
	let size = CCArray.length mt in
	let rec loop x y acc =
		if y = size then
			acc
		else if x = size then
			loop 0 (y + 1) acc
		else
			loop (x + 1) y (f x y acc)
	in
	loop 0 0 acc

let to_string mt f =
	let row_to_str row f =
		CCArray.fold_left (fun str v ->
			Printf.sprintf "%s%s," str (f v)
		) "" row
		|> Shared.rm_last_char
	in
	CCArray.fold_left (fun str row ->
		Printf.sprintf "%s[%s]\n" str (row_to_str row f)
	) "" mt 