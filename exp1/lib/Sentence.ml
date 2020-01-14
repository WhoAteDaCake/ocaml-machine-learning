let split_sentences words =
	let open Token in
	let rec split ls acc = function
	| [] ->
		let all = if List.length acc = 0 then ls else (List.rev acc)::ls in
		List.rev all
	| Text(s)::xs -> split ls (Text(s)::acc) xs
	| Separator(s)::xs -> split (List.rev (Separator(s)::acc)::ls)  [] xs
	| c::xs -> split ls (c::acc) xs
	in split [] [] words

let sentence_to_string ss =
	List.fold_left (fun txt token -> txt ^ " " ^ (Token.to_str token)) "" ss

let doc_to_string ss =
	List.fold_left (fun a b -> a ^ (sentence_to_string b) ^ "\n>" ) ">" ss
