type 'a t = {
  access : ('a, int) Hashtbl.t;
  revers : (int, 'a) Hashtbl.t;
  mutable index : int;
}

let create n index =
  {access = Hashtbl.create n; revers = Hashtbl.create n; index}


let memA tbl item = Hashtbl.mem tbl.access item
let memI tbl indx = Hashtbl.mem tbl.revers indx

let push tbl item =
  try
    Hashtbl.find tbl.access item
  with Not_found -> (
    let index = tbl.index in
    tbl.index<-tbl.index+1;
    Hashtbl.add tbl.access item index;
    Hashtbl.add tbl.revers index item;
    index
  )


let pull tbl indx =
  if Hashtbl.mem tbl.revers indx
  then Hashtbl.find tbl.revers indx
  else failwith "[ocaml-tools/h2Table:pull] - undefined index"

let length tbl =
  Hashtbl.length tbl.access

let iter tbl fonc =
  Hashtbl.iter fonc tbl.access

let map tbl fonc =
  let stack = ref [] in
  let push obj = stack := obj::(!stack) in
  Hashtbl.iter (fun key obj -> push(fonc key obj)) tbl.revers;
  !stack

let mapreduce tbl init map reduce =
  let stack = ref init in
  let push obj = stack := reduce obj !stack in
  Hashtbl.iter (fun key obj -> push(map key obj)) tbl.revers;
  !stack

let strdump (dumpA : 'a -> Tree.stree) (tbl : 'a t) : Tree.stree =
  let stack = ref [] in
  let push obj = stack := obj::(!stack) in
  Hashtbl.iter (fun key objA -> push (Tree.Node [STree.of_int key; dumpA objA])) tbl.revers;
  Tree.Node ((STree.of_int tbl.index)::(!stack))

let strload hsize (loadA : Tree.stree -> 'a) : Tree.stree -> 'a t = function
  | Tree.Node (index::table) ->
  (
    let index = STree.to_int index in
    let access = Hashtbl.create hsize
    and revers = Hashtbl.create hsize in
    List.iter (function
      | Tree.Node [index; item] ->
      (
        let index = STree.to_int index
        and item  = loadA item in
        Hashtbl.add access item index;
        Hashtbl.add revers index item;
      )
      | _ -> assert false) table;
    {access; revers; index}
  )
  | _ -> assert false


