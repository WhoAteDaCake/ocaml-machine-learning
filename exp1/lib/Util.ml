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

let file_path = "/home/augustinas/open-source/mine/machine-learning/exp1/dataset/fake/test.csv"
let file_path = "/home/augustinas/open-source/mine/machine-learning/exp1/dataset/hansard/3.csv"

let row_to_str row f =
		CCList.fold_left (fun str v ->
			Printf.sprintf "%s%s," str (f v)
		) "" row
		|> Shared.rm_last_char

let times = ref []

(* We always reverse it as we need to compare to the lastest time *)
let track_time label a = match CCList.rev !times with
| [] ->
	let _ = times := (Unix.time ())::(!times) in
	a
| x::_ ->
	let now = Unix.time () in
	let _ = Printf.printf "[%d][%s] Time elapsed %s\n" (CCList.length !times) label (Shared.long_float_str (now -. x)) in
	let _ = times := now::(!times) in
	a

let run () =
	let pairs = Helpers.csv_to_pairs file_path in
	let corpus = pairs
		|> track_time "Pairs"
		|> Prepare.corpus
		|> track_time "Prepare"
		|> Text.calculate_corpus_freqs
		|> track_time "Corpus freqs"
		|> Text.calculate_corpus_idfs
		|> track_time "Idfs"
		|> Text.calculate_corpus_vectors 2.0 0.75
		|> track_time "Vectors"
		|> Text.calculate_corpus_distances
		|> track_time "Distance matrix"
	in
	let ids_to_names ids =
		CCList.map (fun idx -> Corpus.document_name_of_index corpus idx) ids
	in
	let matrix = Corpus.distances_exn corpus in
	let (clusters, noise) = DBScan.run 0.1 2 matrix in
	let _ = track_time "Clusters" () in
	let _ = print_endline "" in
	let _ = IntMap.iter (fun k ls ->
		Printf.printf "(%d) [%s]" k (row_to_str (ids_to_names ls) Shared.identity)
	) clusters
	in
	let _ = Printf.printf "\nNoise (%d): [%s] \n\n" (CCList.length noise) (row_to_str (noise |> ids_to_names) Shared.identity) in
  let mtx_str = Matrix.to_string matrix Shared.long_float_str in
  let _ = print_endline mtx_str in
  ()