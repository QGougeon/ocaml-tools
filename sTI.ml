open Extra
open Tree

let unpack_text text =
  let x = String.get text 0 in
  let y = String.sub text 1 (String.length text - 2) in
  let z = String.get text (String.length text - 1) in
  assert(x = '"');
  assert(z = '"');
  Scanf.unescaped y

let load_leaf strlist =
  let rec aux carry = function
    | []  -> assert false
    | head::tail -> match head with
      | "]"  -> Leaf (String.concat " " (List.rev carry)), tail
      | text  -> aux ((unpack_text text)::carry) tail
  in
  aux [] strlist

let rec load_tree strlist =
  let rec aux carry = function
    | []  -> assert false
    | head::tail -> match head with
      | ")"  -> Node (List.rev carry), tail
      | "("  -> let tree, tail' = load_tree tail in aux (tree::carry) tail'
      | "["  -> let leaf, tail' = load_leaf tail in aux (leaf::carry) tail'
      | text  -> aux ((Leaf (unpack_text text))::carry) tail
  in aux [] strlist

let load text =
  let strlist = List.filter (function "" -> false | _ -> true) (StrUtil.split ' ' (StrUtil.explode text ||> (function ' ' | '\n' | '\t' -> ' ' | x -> x) |> StrUtil.implode)) in
  let rec aux carry = function
    | [] -> List.rev carry
    | head::tail -> match head with
      | "("  -> let tree, tail' = load_tree tail in aux (tree::carry) tail'
      | "["  -> let leaf, tail' = load_leaf tail in aux (leaf::carry) tail'
      | text  -> aux ((Leaf (unpack_text text))::carry) tail
  in aux [] strlist

let stream_to_stree stream =
  let pull () =
    try        Some(Stream.next stream)
    with _ -> None
  in
  let blank = function ' ' | '\t' | '\n' -> true | _ -> false in
  let parse_word () =
    let rec aux carry = match pull () with
      | None -> failwith "[ocaml-tools/strTreeParser:str_tree_parser:parse_word] parsing error"
      | Some head -> match head with
        | '\\' -> let c = pull () |> Tools.unop in aux ('\\'::c::carry)
        | '"'  -> Scanf.unescaped (StrUtil.implode (List.rev carry))
        |  c   -> aux (c::carry)
    in aux []
  in
  let parse_leaf () =
    let rec aux carry = match pull () with
      | None -> failwith "[ocaml-tools/strTreeParser:str_tree_parser:parse_leaf] parsing error : 0"
      | Some head -> match head with
        | '"' -> let word = parse_word () in aux (word::carry)
        | ']' -> String.concat " " (List.rev carry)
        |  c when blank c -> aux carry
        | _ -> failwith "[ocaml-tools/strTreeParser:str_tree_parser:parse_leaf] parsing error : 1"
    in aux []
  in
  let rec parse_node () =
    let rec aux carry = match pull () with
      | None -> List.rev carry
      | Some head ->
      ( match head with
        | '(' -> let anode = parse_node () in aux ((Node anode)::carry)
        | '[' -> let aleaf = parse_leaf () in aux ((Leaf aleaf)::carry)
        | '"' -> let aword = parse_word () in aux ((Leaf aword)::carry)
        | ')' -> List.rev carry
        |  c when blank c -> aux carry
        | _ -> failwith "[ocaml-tools/strTreeParser:str_tree_parser:parse_node] parsing error"
      )
    in aux []
  in
  parse_node ()

let loadfile target =
  let file = open_in target in
  let stree = file |> Stream.of_channel |> stream_to_stree in
  close_in file;
  stree
