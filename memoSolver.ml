module type MODELE =
sig
  type prb
  type sol
  type coprb
  type coprb'
  type subprb
  type subprb'
  type subsol

  val presolver : prb -> (sol, coprb * subprb) result
  val solver : subprb -> subsol
  val postsolver : coprb -> subsol -> sol
  val dump_subprb : subprb -> subprb'
  val load_subprb : subprb' -> subprb
  val dump_subsol : subsol -> subsol'
  val load_subsol : subsol' -> subsol

  val __check_reverse__ : bool
end

type ('prb, 'sol, 'coprb, 'coprb', 'subprb, 'subprb', 'subsol) t = {
  presolver   : 'prb -> ('sol, 'coprb * 'subprb) Utils.merge;
  solver      : ('prb -> 'sol) -> 'subprb -> 'subsol;
  postsolver  : 'coprb -> 'subsol -> 'sol;
  dump_subprb : 'subprb -> 'subprb';
  load_subprb : 'subprb' -> 'subprb;
  dump_subsol : 'subsol -> 'subsol';
  load_subsol : 'subsol' -> 'subsol;
  map         : 'prb -> 'sol;
  table       : ('subprb', 'subsol') Hashtbl.t;
  hitCnt : int ref;
  clcCnt : int ref;
}

let create n = {
  table = Hashtbl.create n;
  hitCnt = ref 0;
  clcCnt = ref 0;
}

let test mem a = Hashtbl.mem (mem.table) a;;
let push mem a b =
  if test mem a
  then failwith "[ocaml-tools/memoSolver:push] - already stored"
  else Hashtbl.add (mem.table) a b

let memo mem a b = push mem a b; b;;
let pull mem a = Hashtbl.find (mem.table) a;;

let apply mem fonc a =
  if test mem a
  then(incr mem.hitCnt; pull mem a)
  else(incr mem.clcCnt; memo mem a (fonc c))

let print_stats mem =
  print_string   "MemoTable's length:\t";
  print_int (Hashtbl.length (mem.table));
  print_string  "\nMemoTable's HitCnt:\t";
  print_int (!(mem.hitCnt));
  print_string  ".\nMemoTable's ClcCnt:\t";
  print_int (!(mem.clcCnt));
  print_string  ".\n"

let dump_stats mem = Tree.(Node [
    Node [Leaf "length:"; STD.int (Hashtbl.length (mem.table))];
    Node [Leaf "hit count:"; STD.int (!(mem.hitCnt))];
    Node [Leaf "clc count:"; STD.int (!(mem.clcCnt))]
  ])

let make n = let mem = create n in ( mem, apply mem )

