let ignore _ = "_"
let bitv = Bitv.L.of_bool_string
let string item : string = item
let unit () = "()"
let bool = function
	| "true"  -> true
	| "false" -> false
	| _ -> assert false
let int = int_of_string
let float = float_of_string
