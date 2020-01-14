let append_acc token acc ls =
  let open Token in
  if String.length acc <> 0 then
    token::(Text acc)::ls
  else
    token::ls

let tokenize text =
  let open Token in
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
		let chars = Shared.explode acc in
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
