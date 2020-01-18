open Helpers

let text = "Dr. Andrew has seen. This is a beat"

let text = "He is playing in the field. He is running towards the football. The football game ended. It started raining while everyone was playing in the field."


let words_to_string ss =
	List.fold_left (fun txt token -> txt ^ token ^ ", ") "" ss

let run () =
  let results = Tokenizer.tokenize text in
  let doct = Sentence.split_sentences results in
  let doc = Vectorise.clean_document doct in
  let all_words = List.concat doc in
  let (w_count, words) = Vectorise.word_counts all_words in
  let _ = Vectorise.tf_idf w_count words doc in
  let str = Sentence.doc_to_string doct in
  let wstr = Sentence.list_join (fun (w,c) -> w) "," (StringMap.to_list words) in
  print_endline wstr
