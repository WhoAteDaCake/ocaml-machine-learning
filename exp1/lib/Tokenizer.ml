open Token
open Modules

let append_acc token acc ls =
  if String.length acc <> 0 then
    token::(Text acc)::ls
  else
    token::ls

let tokenize text =
  let rec split (tokens: Token.t list) acc = function
  | [] -> List.rev tokens
  | ' '::xs -> split (append_acc WhiteSpace acc tokens) "" xs
  | '\r'::'\n'::xs -> split (append_acc CRNewLine acc tokens) "" xs
  | '\n'::xs -> split (append_acc NewLine acc tokens) "" xs
  | '?'::'!'::xs -> split (append_acc (Separator "?!")  acc tokens) "" xs
  | '?'::xs -> split (append_acc (Separator "?")  acc tokens) "" xs
  | '!'::xs -> split (append_acc (Separator "!")  acc tokens) "" xs
  | '.'::xs ->
	begin match xs with
	| [] -> split (append_acc (Separator ".") acc tokens) "" []
	| ' '::xss ->
		let chars = CCString.to_list acc in
		(* let _ = print_endline acc in *)
		if Shared.is_abbreviation chars || Shared.is_number chars then
			split (WhiteSpace::Text(acc ^ ".")::tokens) "" xss
		else 
			split (WhiteSpace::Separator(".")::Text(acc)::tokens) "" xss
	| c::xss -> split tokens (acc ^ "." ^ String.make 1 c) xss 
	end
  | c::xs -> split tokens (acc ^ String.make 1 c) xs
  in
  split [] "" (Shared.explode text)

let split_sentences words =
  let rec split ls acc = function
  | [] ->
    let all = if List.length acc = 0 then ls else (List.rev acc)::ls in
    List.rev all
  | Separator(s)::xs -> split (List.rev (Separator(s)::acc)::ls) [] xs
  | c::xs -> split ls (c::acc) xs
  in
  split [] [] words

let rec words_only ?lower_case:(lower_case=true) words =
  let fn =
    if lower_case then
      CCString.lowercase_ascii
    else
      Helpers.identity
  in
  let rec loop acc = function
  | [] -> List.rev acc
  | Text(s)::xs -> loop ((fn s)::acc) xs
  | _::xs -> loop acc xs
  in loop [] words