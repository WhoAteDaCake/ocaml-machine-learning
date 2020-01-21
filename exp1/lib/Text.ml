open Modules

type document = {
	text: string;
	size: int;
	words: string list;
	freqs: int StringMap.t;
	vectors: (float StringMap.t) option
}

let count word map = match StringMap.find_opt word map with
| None -> 0
| Some c -> c

let word_freqs = 
	CCList.fold_left (fun map word ->
		StringMap.add word ((count word map) + 1) map
	) StringMap.empty

let combin_freqs = CCList.fold_left (fun gfqs fqs ->
	StringMap.fold (fun k v gfqs ->
		StringMap.add k ((count k gfqs) + v) gfqs
	) fqs gfqs 
) StringMap.empty

(*
	REF: https://github.com/kerryrodden/tiny-tfidf/blob/master/src/Corpus.js#L91
*)
let freq_weights doc_n freq =
	let idf_base = log (float_of_int (doc_n + 1)) in
	StringMap.fold (fun k v map ->
		StringMap.add k (idf_base -. log (float_of_int v)) map
	) freq StringMap.empty

(* https://github.com/kerryrodden/tiny-tfidf/blob/master/src/Corpus.js#L122 *)
let document_vectors k1 b docs term_weights =
	let total_size = CCList.fold_left (fun sum doc -> sum + doc.size) 0 docs
		|> float_of_int
	in
	let avg_size = total_size /. (CCList.length docs |> float_of_int) in
	CCList.map (fun doc ->
		let ndl = (doc.size |> float_of_int) /. avg_size in
		let vectors = StringMap.fold (fun term idf acc_map ->
			let combined_weight = match StringMap.find_opt term doc.freqs with
			| None -> 0.0
			| Some(tf) ->
				let tf = float_of_int tf in
				(idf *. tf *. (k1 +. 1.0)) /. (k1 *. (1.0 -. b +. b *. ndl) +. tf)
			in
			(* const cw = tf ? (idf * tf * (K1 + 1)) / (K1 * (1 - b + b * ndl) + tf) : 0.0; *)
			StringMap.add term combined_weight acc_map
		) term_weights StringMap.empty
		in
		{ doc with vectors = Some vectors }
	) docs
