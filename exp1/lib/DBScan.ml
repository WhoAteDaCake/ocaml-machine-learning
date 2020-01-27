type status =
| Cluster of int 
| Noise
| Untouched

let run eps min_pts matrix =
	let size = Matrix.size matrix in
	let status = CCArray.init size (fun _ -> Untouched) in
	let clusters = CCVector.make 0 [] in
	let add_to_cluster idx c_idx =
		let _ = CCVector.set clusters c_idx (idx::(CCVector.get clusters c_idx)) in
		status.(idx) <- Cluster c_idx
	in
	(* Get neighbours surounding an index*)
	let get_region_neighbours idx = 
		CCArray.fold_left (fun (i, acc) _ -> 
			let nbs =
				if i <> idx && (Matrix.get matrix idx i) <= eps then
					i::acc
				else acc
			in
			(i + 1, nbs)
		) (0, []) status |> snd
	in
	(* Expand cluster *)
	let rec expand_cluster idx nbs c_idx =
		let _ = add_to_cluster idx c_idx in
		CCList.iter (fun nb_idx ->
			let _ =
				if status.(nb_idx) = Untouched then
					let _ = status.(nb_idx) <- Noise in
					let new_nbs = get_region_neighbours nb_idx in
					if CCList.length new_nbs >= min_pts then
						expand_cluster nb_idx new_nbs c_idx
					else ()
				else
					()
		  in
		  match status.(nb_idx) with
		  | Noise
		  | Untouched -> add_to_cluster nb_idx c_idx
		  | Cluster _ -> () 
		) nbs
	in
	(* Main loop *)	
	let _ = CCArray.iteri (fun i st ->
		if st = Untouched then
			(* Mark noise by default *)
			let _ = status.(i) <- Noise in
			let nbs = get_region_neighbours i in
			if CCList.length nbs >= min_pts then
				let c_idx = CCVector.size clusters in
				let _ = CCVector.push clusters [] in
				expand_cluster i nbs c_idx
			else ()
		else 
			()
	) status
	in
	CCVector.to_list clusters