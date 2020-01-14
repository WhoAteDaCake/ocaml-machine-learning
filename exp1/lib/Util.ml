open Helpers

let text = "Dr. Andrew has seen. This is a beat"

let text = "He is playing in the field. He is running towards the football. The football game ended. It started raining while everyone was playing in the field."


let words_to_string ss =
	List.fold_left (fun txt token -> txt ^ token ^ ", ") "" ss

let run () =
  let results = Tokenizer.tokenize text in
  let doc = Sentence.split_sentences results in
  let doc = Vectorise.clean_document doc in
  let all_words = List.concat doc in
  let (w_count, words) = Vectorise.word_counts all_words in
  (* 
  let _ = print_endline (string_of_int w_count) in
  let _ = StringMap.iter (fun k c -> print_endline (Printf.sprintf "key: %s, value %d" k c)) words in *)
  (* let doc_str = List.fold_left (fun txt sentence -> txt ^ (words_to_string sentence) ^ "\n" ) "" doc in *)
(*   let unq = Helpers.uniq (List.concat doc) in
  let doc_str = List.fold_left (fun txt sentence -> txt ^ (words_to_string sentence) ^ "\n" ) "" doc in
  let doc_str = List.fold_left (fun txt word -> txt ^ word ^ "," ) "" unq in *)
  (* print_endline doc_str *)
  ()