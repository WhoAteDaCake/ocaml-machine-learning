open Modules


let text = "He is playing in the field. He is running towards the football. The football game ended. It started raining while everyone was playing in the field."
let texts = [
  "doc1", "This is test document number 1. It is quite a short document.";
  "doc2", "This is test document 2. It is also quite short, and is a test.";
  "doc3", "Test document number three is a bit different and is also a tiny bit longer."
]

let k1 = 2.0
let b = 0.75

let get_similarity id1 id2 mtx dict =
	let get id = StringMap.find id dict |> Document.index
	in
	Matrix.get mtx (get id1) (get id2)

let analyse text index =
	let words = Tokenizer.words_only (Tokenizer.tokenize text) in
	let freqs = Text.word_freqs words in
	let output: Document.t = {
		words;
		freqs;
		text;
		index;
		size = CCString.length text;
		vectors = None;
	} in
	output

let analyse_corpus entries = 
	let (docs, size) = CCList.fold_left (
		fun (map, c) (key, text) -> (StringMap.add key (analyse text c) map, c + 1)
	) (StringMap.empty, 0) entries in
	let freqs_cmb = Text.corpus_freqs docs in
	(* Combine document terms and calculate their inverse frequencies *)
	let weights = Text.calc_idf size freqs_cmb in
	let docs = Text.document_vectors k1 b docs weights in
  let matrix = Text.distances docs in
  (docs, matrix)

let file_path = "/home/augustinas/open-source/mine/machine-learning/exp1/dataset/hansard/1.csv"

let row_to_str row f =
		CCArray.fold_left (fun str v ->
			Printf.sprintf "%s%s," str (f v)
		) "" row
		|> Helpers.rm_last_char

let csv_to_pairs fname = 
	let file = Csv.load fname in
	(* Remove last *)
	let rows = Csv.to_array file |> CCArray.to_list |> CCList.tl in
	CCList.fold_left (fun acc xs ->
		match CCArray.to_list xs with
		| k::t::xs -> (k, t)::acc 
		| _ -> acc
	) [] rows

let run () =
	(* let file = Csv.load file_path in
	(* Remove last *)
	let rows = Csv.to_array file |> CCArray.to_list |> CCList.tail in
	let _ = row_to_str (rows.(0)) (fun a -> a) |> print_endline in *)
	let pairs = csv_to_pairs file_path in
	let (docs, matrix) = analyse_corpus pairs in
  let mtx_str = Matrix.to_string matrix Helpers.long_float_str in
  let _ = print_string mtx_str in
  ()