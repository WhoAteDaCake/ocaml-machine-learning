open Modules 

type status =
| Noise
| Cluster of int
| Untouched

let uniq l1 l2 =
	let set = IntSet.of_list l1 in
	let set = IntSet.add_list set l2 in
	IntSet.to_list set

let run eps min_pts matrix =
	let size = Matrix.size matrix in
	let c_idx = ref 0 in
	let status = CCArray.make size Untouched in
	let get_neighbours idx = 
		(* Matrix is symetric, so the fold direction doesn't matter *)
		let items = Matrix.fold_x matrix (fun x y acc ->
			if x <> y && Matrix.get matrix x y <= eps then
					x::acc
			else acc
		) [] idx
		in
		items
	in
	let _ = CCArray.iteri (fun idx _ ->
		if status.(idx) <> Untouched then
			()
		else
			let nbs = get_neighbours idx in
			(* Not enough neighbours, mark as noise and continue *)
			if CCList.length nbs < min_pts then
				status.(idx) <- Noise
			else
				(* New cluster found *)
				let _ = c_idx := !c_idx + 1 in
				(* Assign the index to own cluster *)
				let _ = status.(idx) <- Cluster !c_idx in
				let rec loop = function
				| [] -> ()
				| s_idx::ls ->
					if status.(s_idx) = Noise then
						let _ = status.(s_idx) <- Cluster !c_idx in
						loop ls
					else if status.(s_idx) <> Untouched then
						loop ls
					else 
						let _ = status.(s_idx) <- Cluster !c_idx in
						let nbs = get_neighbours s_idx in
						if CCList.length nbs < min_pts then
							loop ls
						else
							loop (uniq nbs ls)
				in
				loop nbs
	) status 
	in
	CCArray.fold_left (fun (idx, map) status -> match status with
	| Cluster c_idx ->
		let map = IntMap.update c_idx (fun ls_opt ->
			match ls_opt with
			| None -> Some (idx::[])
			| Some ls -> Some (idx::ls)
		) map 
		in
		(idx + 1, map) 
	| _ -> (idx + 1, map)
	) (0, IntMap.empty) status