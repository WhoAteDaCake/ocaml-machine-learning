open Modules

(* let run () =
	let mx = Matrix.empty 10 10 0.0 in
	let mx_str = Matrix.to_string ~show_index:true mx string_of_float in
	print_endline mx_str *)


let text = "He is playing in the field. He is running towards the football. The football game ended. It started raining while everyone was playing in the field."
let texts = [
  "This is test document number 1. It is quite a short document.";
  "This is test document 2. It is also quite short, and is a test.";
  "Test document number three is a bit different and is also a tiny bit longer."
]

let k1 = 2.0
let b = 0.75

let analyse text =
	let words = Tokenizer.words_only (Tokenizer.tokenize text) in
	let freqs = Text.word_freqs words in
	let output: Text.document = { words; freqs; text; size = CCString.length text; vectors = None; } in
	output

let run () =
	let docs = List.map analyse texts in
	let freqs_cmb = (List.map (fun (d: Text.document) -> d.freqs) docs) 
		|> Text.combin_freqs
	in
	(* Combine document terms and calculate their inverse frequencies *)
	let weights = Text.freq_weights (List.length docs) freqs_cmb in
	let _ = Text.document_vectors k1 b docs weights in
	let _ = StringMap.iter (fun k v -> Printf.printf "%s, %f\n" k v) weights in
(* 	let _ = StringMap.iter (fun k v -> Printf.printf "%s, %d\n" k v) freqs in
	let _ = print_endline "---------------" in
	let _ = StringMap.iter (fun k v -> Printf.printf "%s, %d\n" k v) freqs_cmb in		 *)
  ()