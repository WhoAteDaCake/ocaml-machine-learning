open Modules

let rm_last_char str =
	CCString.to_list str
	|> CCList.take ((CCString.length str) - 1)
	|> CCString.of_list

let list_join f ch xs =
	List.fold_left (fun txt x -> txt ^ (f x) ^ ch ) "" xs
	|> rm_last_char