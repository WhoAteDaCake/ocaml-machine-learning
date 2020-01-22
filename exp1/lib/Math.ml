open Modules
(*
	Assumes vectors are of equal distance
	https://en.wikipedia.org/wiki/Cosine_similarity
*)
let cosine_similarity vec1 vec2 =
	(* dot_product *)
	let (dp, ss1, ss2) = CCList.fold_left2 (fun (dp, ss1, ss2) v1 v2 ->
		if v1 = 0.0 && v2 = 0.0 then
			(dp, ss1, ss2)
		else
			(
				dp +. (v1 *. v2),
				ss1 +. (v1 *. v1),
				ss2 +. (v2 *. v2)
			)
	) (0.0, 0.0, 0.0) vec1 vec2
	in
	let magnitude = (sqrt ss1) *. (sqrt ss2) in
	if magnitude = 0.0 then
		0.0
	else
		dp /. magnitude