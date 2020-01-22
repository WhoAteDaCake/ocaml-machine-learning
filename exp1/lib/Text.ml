open Modules

let word_freqs = 
	CCList.fold_left (fun map term ->
		let count = match StringMap.find_opt term map with
		| None -> 1
		| Some c -> 1 + c
		in
		StringMap.add term count map
	) StringMap.empty

let corpus_freqs map = StringMap.fold (fun _ doc fmap ->
	(* Combine frequencies into a single map *)
	StringMap.fold (fun term term_c fmap ->
		(* Add local frequencies to combined map *)
		let count = match StringMap.find_opt term fmap with
		| None -> term_c
		| Some c -> term_c + c
		in
		StringMap.add term count fmap
	) (Document.freqs doc) fmap
) map StringMap.empty

(*
	REF: https://github.com/kerryrodden/tiny-tfidf/blob/master/src/Corpus.js#L91
	Calculates inverse document frequency weights for a map of terms
	TODO: 
		Check if adding + 1 to doc count is worth it.This would then make term very small
		instead of 0, so it can later be retrieved
*)
let calc_idf doc_n freq =
	let idf_base = log (float_of_int doc_n) in
	StringMap.fold (fun word count map ->
		StringMap.add word (idf_base -. (float_of_int count |> log)) map
	) freq StringMap.empty

(*
	https://github.com/kerryrodden/tiny-tfidf/blob/master/src/Corpus.js#L122
	In this approach we choose to use document size over term counts
	As we sum and later normalize the counts, the unit used (terms or document size)
	doesn't matter too much 
	k1 - Influence of term frequency
		When k1 = 0, term frequency has no influence
		Generally 2 is found to be quite effective
	b - Float in range of [0.0, 1.0]
		When b = 1, assumption is that the documents are long because they are repetitive
		When b = 0, assumption is that the documents are long bececause they are multi-topic
*)
let document_vectors k1 b docs term_weights =
	let (total_size, doc_count) = StringMap.fold (
		fun _ doc (sum, count) -> (sum + Document.size doc, count + 1)
	) docs (0, 0)
	in
	(*
		A term that occurs the same number of times in a short document
		and in a long one is likely to be more valuable for the former
	*)
	let avg_size = (float_of_int total_size) /. (float_of_int doc_count) in
	StringMap.fold (fun identifier doc doc_map ->
		(*
			This has the advantage that the units in which DL is counted do not matter much. A very simple
			measure such as number of characters in d(j) can be quite adequate as a substitute for number
			of term occurrences.
		*)
		let ndl = (Document.size doc |> float_of_int) /. avg_size in
		let vectors = StringMap.fold (fun term idf acc_map ->
			let combined_weight = match StringMap.find_opt term doc.freqs with
			| None -> 0.0
			| Some(tf) ->
				let tf = float_of_int tf in
				(idf *. tf *. (k1 +. 1.0)) /. (k1 *. (1.0 -. b +. b *. ndl) +. tf)
			in
			StringMap.add term combined_weight acc_map
		) term_weights StringMap.empty
		in
		StringMap.add identifier { doc with vectors = Some vectors } doc_map
	) docs StringMap.empty

let distances corpus =
	let vectors = StringMap.fold (fun key doc acc ->
		(Document.vectors_exn doc)::acc
	) corpus []
	in
	let size = CCList.length vectors in 
	let matrix = Matrix.make size size 0.0 in
	let matrix = Matrix.fold matrix (fun x y acc ->
		(*
			Distance from self is always 0
			And matrix is symetric, so if value is set
			we already calculated the similarity
		*)
		if x = y || (Matrix.get acc x y) <> 0.0 then
			acc
		else 
			(* Extract values of a vector *)
			let values n =
					CCList.nth vectors n
					|> StringMap.to_list
					|> CCList.map snd
			in
			let distances =
				Math.cosine_similarity (values x) (values y)  
			in
			(* The matrix is symetric *)
			let acc = Matrix.set acc x y (1.0 -. distances) in
			Matrix.set acc y x (1.0 -. distances) 
	) matrix in
	matrix
	