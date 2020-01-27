open Modules

type t = {
	documents: Document.t StringMap.t;
	index_map: string IntMap.t;
	size: int;
	similarity: (float Matrix.t) option;
	freqs: (int StringMap.t) option;
	idfs: (float StringMap.t) option;
}

let documents { documents; _ } = documents

let index_map { index_map; _ } = index_map

let size { size; _ } = size

let similarity { similarity; _ } = similarity

let similarity_exn { similarity; _ } = match similarity with
| None -> raise (Failure "Similarities not calculated")
| Some mtx -> mtx

let freqs { freqs; _ } = freqs

let freqs_exn { freqs; _ } = match freqs with
| None -> raise (Failure "Frequencies not found")
| Some a -> a

let idfs { idfs; _ } = idfs

let document_of_index corpus index =
	let id = IntMap.find index corpus.index_map in
	StringMap.find id corpus.documents

(* let similarity_between corpus doc1 doc2 =
	let idx_of id = 
		let doc = StringMap.find id corpus.documents in
		Document.index doc
	in
	let mtx = match corpus.similarity with
	| None -> raise (Failure "Similarities not calculated") *)

let document_similarities corpus doc =
	let mtx = similarity_exn corpus in
	let idx = 
		let doc = StringMap.find doc corpus.documents in
		Document.index doc
	in
	let similarities = Matrix.fold_x mtx (fun x y acc ->
		(IntMap.find x corpus.index_map, Matrix.get mtx x y)::acc
	) [] idx
	in
	CCList.sort (fun (_, a) (_, b) -> compare a b) similarities