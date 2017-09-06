open Tree

let leaf map item = Leaf(map item)

let string = leaf StrDump.string
let bool   = leaf StrDump.bool
let int    = leaf StrDump.int
let float  = leaf StrDump.float
let bitv_bool = leaf StrDump.bitv_bool
let bitv_hexa = leaf StrDump.bitv_hexa
let option dump = function
  | None      -> Tree.Node []
  | Some some -> Tree.Node [dump some]
let list dump list = Tree.Node (List.map dump list)
let array dump array = list dump (Array.to_list array)
let unit () = Tree.Node []
let pair dumpA dumpB (a, b) = Tree.Node [dumpA a; dumpB b]
let trio dumpA dumpB dumpC (a, b, c) = Tree.Node [dumpA a; dumpB b; dumpC c]
