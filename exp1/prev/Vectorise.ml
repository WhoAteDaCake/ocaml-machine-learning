open Helpers


let empty_matrix x y default =
	let rec fill f map = function
	| -1 -> map
	| n ->  fill f (IntMap.add n (f ()) map) (n - 1)
	in
	fill (fun () -> fill (fun () -> default) IntMap.empty x) IntMap.empty y  

let words_only ss =
	let open Token in
	List.fold_left (
		fun ss t -> match t with
		| Text(s) -> (String.lowercase_ascii(s))::ss
		| _ -> ss
		) [] ss

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

let tf_idf uniq_words_c uniq_words doc =
	let doc_n = List.length doc in
	let matrix = empty_matrix uniq_words_c doc_n 0.0 in
	let (matrix, _) = StringMap.fold (
		(* Run for each document on the index of the word *)
		fun word _c (mx, i) ->
			let mx = IntMap.fold (fun k v new_mtrix ->
				let word_c = StringMap.find word uniq_words |> float_of_int in
				let idf = log ((float_of_int doc_n) /. word_c) in 
				let row = IntMap.add i idf v in
				IntMap.add k row new_mtrix
			) mx IntMap.empty in
			(mx, i + 1)
	) uniq_words (matrix, 0) in
	let str = Matrix.to_string matrix string_of_float in
	print_endline str
