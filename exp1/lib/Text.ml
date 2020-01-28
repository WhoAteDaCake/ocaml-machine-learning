open Modules

let word_freqs = 
	CCList.fold_left (fun map term ->
		let count = match StringMap.find_opt term map with
		| None -> 1
		| Some c -> 1 + c
		in
		StringMap.add term count map
	) StringMap.empty

let calculate_corpus_freqs corpus =
	(* Outer loop for documents *)
	let freqs = StringMap.fold (fun _ doc t_map ->
		(* Calculate the frequencies *)
		StringMap.fold (fun term term_c t_map ->
			(* Find if a term is already in the document *)
			let count = match StringMap.find_opt term t_map with
			| None -> term_c
			| Some c -> term_c + c
			in StringMap.add term count t_map
		) (Document.freqs doc) t_map
	) (Corpus.documents corpus) StringMap.empty
	in
	{ corpus with freqs = Some freqs; }

(*
	REF: https://github.com/kerryrodden/tiny-tfidf/blob/master/src/Corpus.js#L91
	Calculates inverse document frequency weights for a map of terms
	TODO: 
		Check if adding + 1 to doc count is worth it.This would then make term very small
		instead of 0, so it can later be retrieved
*)
let calculate_corpus_idfs corpus =
	let idfs = match Corpus.freqs corpus with
	| None -> None
	| Some freq ->
		let norm n = float_of_int n |> log in
		let idf_base = Corpus.size corpus |> norm in
		let idfs = StringMap.fold (fun word count map ->
			StringMap.add word (idf_base -. (norm count)) map
		) freq StringMap.empty 
		in Some idfs
	in
	{ corpus with idfs }

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
let calculate_corpus_vectors k1 b corpus =
	let documents = match Corpus.idfs corpus with
	| None -> Corpus.documents corpus
	| Some idfs ->
		let docs = Corpus.documents corpus in 
		let total_size = StringMap.fold (
			fun _ doc sum -> sum + Document.size doc
		) docs 0
		in
		(*
			A term that occurs the same number of times in a short document
			and in a long one is likely to be more valuable for the former
		*)
		let avg_size = (float_of_int total_size) /. (Corpus.size corpus |> float_of_int) in
		let documents = StringMap.fold (fun identifier doc doc_map ->
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
					(idf *. tf *. (k1 +. 1.0)) /. (k1 *. ((1.0 -. b) +. (b *. ndl)) +. tf)
				in
				StringMap.add term combined_weight acc_map
			) idfs StringMap.empty
			in
			StringMap.add identifier { doc with vectors = Some vectors } doc_map
		) docs StringMap.empty
		in documents
	in { corpus with documents }

let get_term_vectors doc1 doc2 = match (Document.vectors doc1, Document.vectors doc2) with
(* Document 1 vectors and document 2 vectors *)
| (Some v1, Some v2) -> 
	(* Assumes that both documents have the same terms, returns list of term weights *)
	StringMap.fold (fun key value1 (vec1, vec2) ->
		let value2 = match StringMap.find_opt key v2 with
		| None -> 0.0
		| Some w -> w
		in
		(value1::vec1, value2::vec2)
	) v1 ([], [])
| _ -> ([], [])

let calculate_corpus_distances corpus =
	let size = Corpus.size corpus in 
	let matrix = Matrix.make size size 0.0 in
	let distances = Matrix.fold matrix (fun x y acc ->
		(*
			Distance from self is always 0
			And matrix is symetric, so if value is set
			we already calculated the similarity
		*)
		if x = y || (Matrix.get acc x y) <> 0.0 then
			acc
		else 
			(* Extract values of a vector *)
			let (v1, v2) = get_term_vectors (Corpus.document_of_index corpus x) (Corpus.document_of_index corpus y) in
			let distance = 1.0 -. (Math.cosine_similarity v1 v2)
			in
			(* The matrix is symetric *)
			let acc = Matrix.set acc x y distance in
			Matrix.set acc y x distance 
	) matrix in
	{ corpus with distances = Some distances }
	