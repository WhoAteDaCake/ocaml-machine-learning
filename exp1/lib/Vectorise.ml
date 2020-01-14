open Helpers

let empty_matrix x y default =
	let rec fill mk map = function
	| -1 -> map
	| n ->  fill mk (IntMap.add n (mk ()) map) (n - 1)
	in
	fill (fun () -> fill (fun () -> default) IntMap.empty x) IntMap.empty y  

let words_only ss =
	let open Token in
	let results = List.fold_left (
		fun ss t -> match t with
		| Text(s) -> (String.lowercase_ascii(s))::ss
		| _ -> ss
		) [] ss
	in
	List.rev results

let clean_document = List.map words_only

let word_counts = 
	List.fold_left
	(fun (count, map) word ->
		let (t_count, w_count) = match StringMap.find_opt word map with
		| None -> (count + 1, 1)
		| Some c -> (count, c + 1)
		in
		(t_count, StringMap.add word w_count map)
	) (0, StringMap.empty)

let tf_idf uniq_c uniq_words doc =
	let doc_n = List.length doc in
	()


(* let tf_idf unique doc =
	let doc_n = List.length doc in
	let uniq_n = List.length unique in
	let empty_matrix = List.map sentences (List.init uniq_n Helpers.identity) in
	let matrix = List.fold_left (fun matrix word -> ) empty_matrix unique
 *)