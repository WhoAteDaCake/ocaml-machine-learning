open Modules

type t = {
	text: string;
	size: int;
	words: string list;
	freqs: int StringMap.t;
	vectors: (float StringMap.t) option;
	index: int;
}

let text { text; _ } = text

let size { size; _ } = size

let words { words; _ } = words

let freqs { freqs; _ } = freqs

let vectors { vectors; _ } = vectors

let vectors_exn { vectors; _ } = match vectors with
| None -> raise Not_found 
| Some vectors -> vectors

let index { index; _ } = index