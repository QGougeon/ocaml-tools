type ('leaf, 'edge, 'node) next =
  | Node of ('leaf, 'edge, 'node) node
  | Leaf of 'leaf
and  ('leaf, 'edge, 'node) edge =
  'edge * ('leaf, 'edge, 'node) next
and  ('leaf, 'edge, 'node) node =
  'node * ('leaf, 'edge, 'node) edge * ('leaf, 'edge, 'node) edge

let get_next_leaf leaf : (_, _, _) next = Leaf leaf
let get_next_node get_node node = Node(get_node node)
let get_edge get_next (edge, next) = (edge, get_next next)
let get_node get_edge0 get_edge1 (node, edge0, edge1) = (node, get_edge0 edge0, get_edge1 edge1)

let get_edge_leaf edge : (_, _, _) edge = get_edge get_next_leaf edge
let get_node_leaf node : (_, _, _) node = get_node get_edge_leaf get_edge_leaf node

let o3_next =
  let dump = Poly.(function
  | Node node -> C2_0 node
  | Leaf leaf -> C2_1 leaf)
  and load = Poly.(function
  | C2_0 node -> Node node
  | C2_1 leaf -> Leaf leaf)
  in (dump, load)

let bindump
  (dump_leaf : 'leaf BinUtils.dump)
  (dump_edge : 'edge BinUtils.dump)
  (dump_node : 'node BinUtils.dump) =
  let rec dump_next' next stream = BinDump.c2 dump_node' dump_leaf (fst o3_next next) stream
  and     dump_edge' = BinDump.pair dump_edge dump_next'
  and     dump_node' = BinDump.trio dump_node dump_edge' dump_edge'
  in
  (dump_next', dump_edge', dump_node')

let bindump_edge leaf edge node =
  let _, edge, _ = bindump leaf edge node in
  edge

let binload
  (load_leaf : 'leaf BinUtils.load)
  (load_edge : 'edge BinUtils.load)
  (load_node : 'node BinUtils.load) =
  let rec load_next' stream = (snd o3_next) (BinLoad.c2 load_node load_leaf stream)
  and     load_edge' = BinLoad.pair load_edge load_next'
  and     load_node' = BinLoad.trio load_node load_edge' load_edge'
  in
  (load_next', load_edge', load_node')

let o3s (dump_leaf, load_leaf) (dump_edge, load_edge) (dump_node, load_node) =
  let dump_next', dump_edge', dump_node' = bindump dump_leaf dump_edge dump_node
  and load_next', load_edge', load_node' = binload load_leaf load_edge load_node in
  ((dump_next', load_next'), (dump_edge', load_edge'), (dump_node', load_node'))

let binload_edge leaf edge node =
  let _, edge, _ = binload leaf edge node in
  edge

let o3s_edge leaf edge node =
  let _, edge, _ = o3s leaf edge node in
  edge
