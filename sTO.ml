open Extra
open Tree

let dump_leaf text =
  match StrUtil.split ' ' (String.escaped text) with
    | []  -> "\"\""
    | [x]  -> "\""^x^"\""
    | lx  -> "[ \""^(String.concat "\" \"" lx)^"\" ]"

let rec dump_tree = function
  | Leaf text -> dump_leaf text
  | Node treelist -> String.concat " " ("("::(treelist ||> dump_tree)@[")"])

let output_tree output_string =
  let rec aux = function
    | Leaf text -> output_string(dump_leaf text)
    | Node treelist -> output_string "( "; List.iter (fun tree -> aux tree; output_string " ") treelist; output_string " )";
  in aux

let output_treelist output_string treelist =
	List.iter (fun tree -> output_tree output_string tree; output_string "\n") treelist

let dump treelist = StrUtil.catmap "\n" dump_tree treelist

let dumpfile treelist target =
  let file = open_out target in
	output_treelist (output_string file) treelist;
  close_out file;
  ()

let print_tree = output_tree print_string
let print_treelist = output_treelist print_string 

let pretty_output_tree output_string =
  let lvlstr lvl text = output_string ((StrUtil.ntimes " " lvl)^text^"\n") in
  let rec aux lvl = function
    | Leaf text -> lvlstr lvl text
    | Node treelist -> lvlstr lvl "("; List.iter (aux(lvl+1)) treelist; lvlstr lvl ")"
  in aux 0

let pprint_v1 treelist =
	List.iter (fun tree -> pretty_output_tree print_string tree; print_newline()) treelist

(* get colored version of str *)
let colorize color str =
  if color > 0
  then "\027[" ^ (string_of_int color) ^ "m" ^ str ^ "\027[0m"
  else str

type enum =
  | T000
  | T001
  | T010
  | T011
  | T100
  | T101
  | T110
  | T111

(* print colored tree *)
let pretty_output_tree_v2 output_string =
  (* draw UTF-8 tree line *)
  let conv = function
    | T000 -> "  "
    | T001 -> "┌ "
    | T010 -> "──"
    | T011 -> "┌─"
    | T100 -> "└ "
    | T101 -> "│ "
    | T110 -> "└─"
    | T111 -> "├─"
  in
  let output_row row =
    output_string (StrUtil.catmap""conv(List.rev row));
  in
  let rec tree row0 rows = function
    | Leaf leaf  -> output_row row0; output_string " "; output_string leaf; output_string "\n";
    | Node liste  -> match liste with
      | []         -> output_row row0; output_string "|\n";
      | [head]     -> tree (T010::row0) (T000::rows) head
      | head::tail -> tree (T011::row0) (T101::rows) head; treelist rows tail
  and treelist row = function
    | head::[]     -> tree (T110::row) (T000::row) head
    | head::tail   -> tree (T111::row) (T101::row) head; treelist row tail
    | []           -> ()
  in List.iter (tree [] [])

let pprint_v2 treelist =
	List.iter (fun tree -> pretty_output_tree_v2 print_string tree; print_newline()) treelist;
