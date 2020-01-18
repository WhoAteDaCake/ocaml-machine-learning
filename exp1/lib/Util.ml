let run () =
	let mx = Matrix.empty 10 10 0.0 in
	let mx_str = Matrix.to_string ~show_index:true mx string_of_float in
	print_endline mx_str