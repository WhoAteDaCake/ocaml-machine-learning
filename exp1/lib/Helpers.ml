open Modules

let csv_to_pairs fname = 
	let file = Csv.load fname in
	(* Remove the headers *)
	let rows = Csv.to_array file |> CCArray.to_list |> CCList.tl in
	CCList.fold_left (fun acc xs ->
		match CCArray.to_list xs with
		| k::t::xs -> (k, t)::acc 
		| _ ->
			let _ = Printf.sprintf "Row is invalid, expected [id, text] column layout" in
			acc
	) [] rows