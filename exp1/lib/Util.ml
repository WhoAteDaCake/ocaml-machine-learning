
let text = "Dr. Andrew has seen. This is a beat"

let run () =
  let results = Tokenizer.tokenize text in
  let str = List.fold_left (fun txt token -> txt ^ " " ^ (Token.to_str token)) "" results in
  print_endline str
