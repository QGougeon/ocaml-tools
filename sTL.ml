open Tree

let leaf map = function
  | Leaf leaf -> map leaf
  | _ -> failwith "[ocaml-tools/sTL:leaf] - parsing error"

let string = leaf StrLoad.string
let bool   = leaf StrLoad.bool
let int    = leaf StrLoad.int
let float  = leaf StrLoad.float
let bitv_bool = leaf StrLoad.bitv_bool
let bitv_hexa = leaf StrLoad.bitv_hexa
let option load = function
  | Node [] -> None
  | Node [some] -> Some(load some)
  | _ -> failwith "[ocaml-tools/sTL:option] - parsing error"
let list load = function
  | Node list -> List.map load list
  | Leaf _ -> failwith "[ocaml-tools/sTL:list] - parsing error"
let array load stree = list load stree |> Array.of_list
let unit = function
  | Node [] -> ()
  | _ -> failwith "[ocaml-tools/sTL:unit] - parsing error"
let pair loadA loadB = function
  | Node [a; b] -> (loadA a, loadB b)
  | _ -> failwith "[ocaml-tools/sTL:pair] - parsing error"
let trio loadA loadB loadC = function
  | Node [a; b; c] -> (loadA a, loadB b, loadC c)
  | _ -> failwith "[ocaml-tools/sTL:trio] - parsing error"
