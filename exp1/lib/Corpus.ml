open Modules

type t = {
	documents: Document.t StringMap.t;
	index_map: string IntMap.t;
	size: int;
	distances: (float Matrix.t) option;
	freqs: (int StringMap.t) option;
	idfs: (float StringMap.t) option;
}

let documents { documents; _ } = documents

let index_map { index_map; _ } = index_map

let size { size; _ } = size

let distances { distances; _ } = distances

let distances_exn { distances; _ } = match distances with
| None -> raise (Failure "Similarities not calculated")
| Some mtx -> mtx

let freqs { freqs; _ } = freqs

let freqs_exn { freqs; _ } = match freqs with
| None -> raise (Failure "Frequencies not found")
| Some a -> a

let idfs { idfs; _ } = idfs

let document_name_of_index corpus index =
	IntMap.find index corpus.index_map

let document_of_index corpus index =
	StringMap.find (document_name_of_index corpus index) corpus.documents

let document_distances corpus doc =
	let mtx = distances_exn corpus in
	let idx = 
		let doc = StringMap.find doc corpus.documents in
		Document.index doc
	in
	let similarities = Matrix.fold_x mtx (fun x y acc ->
		(IntMap.find x corpus.index_map, Matrix.get mtx x y)::acc
	) [] idx
	in
	CCList.sort (fun (_, a) (_, b) -> compare a b) similarities