
let text = "Dr. Andrew has seen. This is a beat"

let text2 = "IL-33 is known to induce the production of Th2-associated
cytokines (e.g. IL-5 and IL-13)."

let run () =
  let results = Tokenizer.tokenize text2 in
  let doc = Sentence.split_sentences results in
  let str2 = Sentence.doc_to_string doc in
  let str = List.fold_left (fun txt token -> txt ^ " " ^ (Token.to_str token)) "" results in
  print_endline str2
