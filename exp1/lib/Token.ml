type t =
| Text of string 
| Separator of string
| WhiteSpace
| NewLine
| CRNewLine

let to_str = function
| Text a -> Printf.sprintf "Token(%s)" a
| Separator a -> Printf.sprintf "Separator(%s)" a
| WhiteSpace -> "WhiteSpace"
| NewLine -> "NewLine"
| CRNewLine -> "CRNewLine"
