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

let file_path = "/home/augustinas/open-source/mine/machine-learning/exp1/dataset/hansard/3.csv"
let file_path = "/home/augustinas/open-source/mine/machine-learning/exp1/dataset/fake/test.csv"

let row_to_str row f =
		CCArray.fold_left (fun str v ->
			Printf.sprintf "%s%s," str (f v)
		) "" row
		|> Shared.rm_last_char

(* let csv_to_pairs fname = 
	let file = Csv.load fname in
	(* Remove last *)
	let rows = Csv.to_array file |> CCArray.to_list |> CCList.tl in
	CCList.fold_left (fun acc xs ->
		match CCArray.to_list xs with
		| k::t::xs -> (k, t)::acc 
		| _ -> acc
	) [] rows
 *)
let run () =
	let pairs = Helpers.csv_to_pairs file_path in
	let corpus = Prepare.corpus pairs
		|> Text.calculate_corpus_freqs
		|> Text.calculate_corpus_idfs
		|> Text.calculate_corpus_vectors 2.0 0.75
		|> Text.calculate_corpus_distances in
	let matrix = Corpus.distances_exn corpus in
	let _ = DBScan.run 0.5 2 matrix in
(* 	let _ = CCList.iteri (fun idx cluster ->
		let _ = Printf.printf "Cluster %d: \n" idx in
		let _ = CCList.iter (fun id -> Printf.printf "%d, " id) cluster in
		print_string "\n\n"
	) clusters
	in *)
  let mtx_str = Matrix.to_string matrix Shared.long_float_str in
  let _ = print_endline mtx_str in
  ()