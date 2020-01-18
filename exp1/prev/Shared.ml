let explode s = List.init (String.length s) (String.get s)

let c_arr_to_str = List.fold_left (fun a b -> a ^ (String.make 1 b)) "" 

let last_char s = match String.length s with
| 0 -> None
| n -> Some (String.get s (n - 1))

let is_capital c =
  let code = Char.code c in
  code >= 65 && code<= 90

let is_letter c =
  let code = Char.code c in
  (code >= 65 && code<= 90) || (code >=97 && code <= 122)

let is_numeric c =
  let code = Char.code c in
  code >= 48 && code <= 57

 (* 10 - \n | 13 - \r | 32 - ' ' | 48 - '0' | 57 - '9' | 65 - A | 90 - Z *)
let rec was_sentence_ended = function
| [] -> false
| c::xs ->
  let code = Char.code c in
  if code = 10 || code = 13 || code = 32 then
    was_sentence_ended xs
  else if is_numeric c || is_capital c then
    true
  else
    false

(*
  We don't end a sentence if it's an abreviation
  It's an abreviation if it's all capitals and dots ?
 *)
let is_abbreviation word =
  let s_word = c_arr_to_str word in
  let has_dot = List.exists (fun c -> c = '.') word in
  let is_period_word = List.exists (fun pw -> pw = s_word) Constants.period_words in
  let is_abbrev = not ( List.exists (fun c -> not (is_letter c || c = '.')) word) in
  is_period_word || (is_abbrev && has_dot)

let is_number word = 
  List.exists (fun c -> is_numeric c || c = '.' || c = ',') word
