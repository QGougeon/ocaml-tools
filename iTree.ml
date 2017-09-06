type 'a iGrove =
	| Leaf of 'a
	| Node of ('a iTree) Iter.iter
and 'a iTree = unit -> 'a iGrove
;;


let map (f : 'a -> 'b) (tree : 'a iTree) : 'a iTree =
	let rec aux (t : 'a iTree) () : 'a iGrove =
		match t() with
		| Leaf e -> Leaf(f e)
		| Node i -> Node(Iter.map aux i)
	in
	
	aux tree
;;

let fold_prefix (f : 'a -> 'b -> 'a) (x0 : 'a) (tree : 'b iTree) : 'a =
	let rec fold (acc : 'a) (t : 'b iTree) : 'a =
		match t() with
		| Leaf e -> f acc e
		| Node i -> Iter.fold_left0 fold acc i
	in
	
	fold x0 tree
;;

let fold_infix_left (fold : 'b -> 'b -> 'b) (convert : 'a -> 'b) (x0 : 'b) (tree : 'a iTree) : 'b =
	let rec aux (t : 'a iTree) : 'b =
		match t() with
		| Leaf e -> convert e
		| Node i -> Iter.fold_left0 (fun x t -> fold x (aux t)) x0 i
	in
	
	aux tree
;;

let fold_infix_right (fold : 'b -> 'b -> 'b) (convert : 'a -> 'b) (tree : 'a iTree) (x0 : 'b) : 'b =
	let rec aux (t : 'a iTree) : 'b =
		match t() with
			| Leaf e -> convert e
			| Node i -> Iter.fold_right0 (fun t x -> fold (aux t) x) i x0
	in
	
	aux tree
;;

let fold_infix_left_complex (fold : 'b -> 'c -> 'b) (convert_leaf : 'a -> 'c) (convert_node : 'b -> 'c) (x0 : 'b) (tree : 'a iTree) : 'c =
	let rec aux (t : 'a iTree) : 'c =
		match t() with
		| Leaf e -> convert_leaf e
		| Node i -> convert_node (Iter.fold_left0 (fun x t -> fold x (aux t)) x0 i)
	in
	
	aux tree
;;

let fold_infix_right_complex (fold : 'c -> 'b -> 'b) (convert_leaf : 'a -> 'c) (convert_node : 'b -> 'c) (tree : 'a iTree) (x0 : 'b) : 'c =
	let rec aux (t : 'a iTree) : 'c =
		match t() with
		| Leaf e -> convert_leaf e
		| Node i -> convert_node (Iter.fold_right0 (fun t x -> fold (aux t) x) i x0)
	in
	
	aux tree
;;


let fold_suffix (f : 'a -> 'b -> 'b) (tree : 'a iTree) (x0 : 'b) : 'b =
	let rec fold (t : 'a iTree) (acc : 'b)  : 'b =
		match t() with
		| Leaf e -> f e acc
		| Node i -> Iter.fold_right0 fold i acc
	in
	
	fold tree x0
;;


		
		
