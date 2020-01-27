open Modules

let document text index =
	let words = Tokenizer.words_only (Tokenizer.tokenize text) in
	let freqs = Text.word_freqs words in
	let output: Document.t = {
		words;
		freqs;
		text;
		index;
		size = CCString.length text;
		vectors = None;
	}
	in output

let corpus entries = 
	let (documents, index_map, size) = CCList.fold_left (fun (doc_map, id_map, c) (key, text) ->
		let doc_map = StringMap.add key (document text c) doc_map in
		let id_map = IntMap.add c key id_map in
		(doc_map, id_map, c + 1)
	) (StringMap.empty, IntMap.empty, 0) entries in
	let corpus: Corpus.t = {
		documents;
		index_map;
		size;
		similarity = None;
		freqs = None;
		idfs = None;
	}
	in
	corpus